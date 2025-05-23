package bloop.io

import java.io.IOException
import java.nio.file.FileVisitResult
import java.nio.file.FileVisitor
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.Promise
import scala.util.control.NonFatal

import bloop.logging.Logger
import bloop.task.Task

import monix.eval.{Task => MonixTask}
import monix.execution.Cancelable
import monix.execution.Scheduler
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.AssignableCancelable
import monix.execution.cancelables.CompositeCancelable
import monix.reactive.Consumer
import monix.reactive.MulticastStrategy
import monix.reactive.Observable
import java.nio.file.AccessDeniedException

object ParallelOps {

  sealed trait CopyMode
  object CopyMode {
    final case object NoReplace extends CopyMode
    final case object ReplaceExisting extends CopyMode
    final case object ReplaceIfMetadataMismatch extends CopyMode
  }

  /**
   * A configuration for a copy process.
   *
   * @param parallelUnits Threads to use for parallel IO copy.
   * @param replaceExisting Whether the copy should replace existing paths in the target.
   * @param denylist A list of both origin and target paths that if matched skip the copy.
   */
  case class CopyConfiguration(
      parallelUnits: Int,
      mode: CopyMode,
      denylist: Set[Path],
      denyDirs: Set[Path],
      skipDotDirectories: Boolean = false
  )

  case class FileWalk(visited: List[Path], target: List[Path])

  private[this] val takenByOtherCopyProcess = new ConcurrentHashMap[Path, Promise[Unit]]()

  /**
   * Copies the resources in `resources` to the directory `copyTo` ,
   */
  def copyResources(
      resources: List[AbsolutePath],
      copyTo: AbsolutePath,
      config: ParallelOps.CopyConfiguration,
      logger: Logger,
      scheduler: Scheduler
  ): Task[Unit] = {
    val (singleFiles, classpathEntries) =
      resources.partition(path => path.exists && path.isFile)

    val singleFilesToCopy =
      for (file <- singleFiles)
        yield file.underlying -> copyTo.underlying.resolve(file.underlying.toFile().getName())

    val classpathEntriesCopy =
      for (entry <- classpathEntries) yield {
        ParallelOps
          .copyDirectories(config.copy(skipDotDirectories = true))(
            entry.underlying,
            copyTo.underlying,
            scheduler,
            enableCancellation = false,
            logger,
            singleFilesToCopy
          )
      }

    Task.gatherUnordered(classpathEntriesCopy).map(_ => ())
  }

  /**
   * Copies files from [[origin]] to [[target]] with the provided copy
   * configuration in parallel on the scheduler [[scheduler]].
   *
   * @param enableCancellation A flag to control whether the task should be
   * cancelled or not. For semantics-preserving copy tasks, we might want to
   * disable cancellation. Otherwise, it's possible that, for example,
   * `BspServer` calls `cancel` on the post-compilation task even though the
   * compilation was cancelled, because the order in which cancel finalizers
   * are done is arbitrary. This value is usually `false` because most of the
   * copies are key for compilation semantics.
   *
   * @return The list of paths that have been copied.
   */
  def copyDirectories(configuration: CopyConfiguration)(
      origin: Path,
      target: Path,
      scheduler: Scheduler,
      enableCancellation: Boolean,
      logger: Logger,
      additionalFiles: Seq[(Path, Path)] = Nil
  ): Task[FileWalk] = Task.defer {
    val isCancelled = AtomicBoolean(false)

    import scala.collection.mutable
    val visitedPaths = new mutable.ListBuffer[Path]()
    val targetPaths = new mutable.ListBuffer[Path]()
    val (observer, observable) = Observable.multicast[((Path, BasicFileAttributes), Path)](
      MulticastStrategy.publish
    )(scheduler)

    val copyAdditionalFiles = Task {
      additionalFiles.foreach {
        case (from, to) =>
          val attributes = Files.readAttributes(from, classOf[BasicFileAttributes])
          observer.onNext(((from, attributes), to))
      }
    }

    val discovery = new FileVisitor[Path] {
      var firstVisit: Boolean = true
      var currentTargetDirectory: Path = target
      def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
        if (isCancelled.get) FileVisitResult.TERMINATE
        else {
          if (
            attributes.isDirectory || configuration.denylist.contains(
              file
            ) || configuration.denyDirs.find(file.startsWith(_)).isDefined
          ) ()
          else {
            val rebasedFile = currentTargetDirectory.resolve(file.getFileName)
            if (configuration.denylist.contains(rebasedFile)) ()
            else {
              visitedPaths.+=(file)
              targetPaths.+=(rebasedFile)
              observer.onNext((file -> attributes, rebasedFile))
            }
          }
          FileVisitResult.CONTINUE
        }
      }

      def visitFileFailed(
          t: Path,
          e: IOException
      ): FileVisitResult = FileVisitResult.CONTINUE

      def preVisitDirectory(
          directory: Path,
          attributes: BasicFileAttributes
      ): FileVisitResult = {
        if (configuration.skipDotDirectories && directory.getFileName.toString.startsWith(".")) {
          FileVisitResult.SKIP_SUBTREE
        } else if (isCancelled.get) FileVisitResult.TERMINATE
        else {
          if (firstVisit) {
            firstVisit = false
          } else {
            currentTargetDirectory = currentTargetDirectory.resolve(directory.getFileName)
          }
          Files.createDirectories(currentTargetDirectory)
          FileVisitResult.CONTINUE
        }
      }

      def postVisitDirectory(
          directory: Path,
          exception: IOException
      ): FileVisitResult = {
        currentTargetDirectory = currentTargetDirectory.getParent()
        FileVisitResult.CONTINUE
      }
    }

    val discoverFileTree = Task {
      if (!Files.exists(origin)) {
        FileWalk(Nil, Nil)
      } else {
        Files.walkFileTree(origin, discovery)
        FileWalk(visitedPaths.toList, targetPaths.toList)
      }
    }.doOnFinish {
      case Some(t) => Task(observer.onError(t))
      case None => Task(observer.onComplete())
    }

    val subscribed = Promise[Unit]()

    // We set the value of this cancelable when we start consuming task
    var completeSubscribers: Cancelable = Cancelable.empty
    val cancelables = new mutable.ListBuffer[Cancelable]()
    val cancelable = AssignableCancelable.multi { () =>
      val tasksToCancel = cancelables.synchronized { cancelables.toList }
      Cancelable.cancelAll(completeSubscribers :: tasksToCancel)
    }

    val copyFileSequentially = Consumer.foreachTask[((Path, BasicFileAttributes), Path)] {
      case ((originFile, originAttrs), targetFile) =>
        def copy(replaceExisting: Boolean, retry: Int = 3): Unit = try {
          if (replaceExisting) {
            Files.copy(
              originFile,
              targetFile,
              StandardCopyOption.COPY_ATTRIBUTES,
              StandardCopyOption.REPLACE_EXISTING
            )
          } else {
            Files.copy(
              originFile,
              targetFile,
              StandardCopyOption.COPY_ATTRIBUTES
            )
          }
          ()
        } catch {
          case _: AccessDeniedException if retry > 0 =>
            logger.warn(s"Could not access file, retrying copying $originFile to $targetFile.")
            copy(replaceExisting, retry - 1)
          case NonFatal(t) =>
            logger.error(
              s"Unexpected error when copying $originFile to $targetFile, you might need to restart the build server.",
              t
            )
        }

        // It's important that this task is not forked for performance reasons
        def triggerCopy(p: Promise[Unit]) = MonixTask.eval {
          try {
            // Skip work if cancellation is on and complete promise in finalizer
            if (isCancelled.get) ()
            else {
              configuration.mode match {
                case CopyMode.ReplaceExisting => copy(replaceExisting = true)
                case CopyMode.ReplaceIfMetadataMismatch =>
                  import scala.util.{Try, Success, Failure}
                  Try(Files.readAttributes(targetFile, classOf[BasicFileAttributes])) match {
                    case Success(targetAttrs) =>
                      val changedMetadata = {
                        originAttrs.lastModifiedTime
                          .compareTo(targetAttrs.lastModifiedTime) != 0 ||
                        originAttrs.size() != targetAttrs.size()
                      }

                      if (!changedMetadata) ()
                      else copy(replaceExisting = true)
                    // Can happen when the file does not exist, replace in that case
                    case Failure(_: IOException) => copy(replaceExisting = true)
                    case Failure(t) => throw t
                  }
                case CopyMode.NoReplace =>
                  if (Files.exists(targetFile)) ()
                  else copy(replaceExisting = false)
              }
            }
          } finally {
            takenByOtherCopyProcess.remove(targetFile)
            // Complete successfully to unblock other tasks
            p.success(())
          }
          ()
        }

        def acquireFile: MonixTask[Unit] = {
          val currentPromise = Promise[Unit]()
          val promiseInMap = takenByOtherCopyProcess.putIfAbsent(targetFile, currentPromise)
          if (promiseInMap == null) {
            triggerCopy(currentPromise)
          } else {
            MonixTask.fromFuture(promiseInMap.future).flatMap(_ => acquireFile)
          }
        }

        acquireFile.coeval(scheduler).value match {
          // The happy path is that we evaluate the task and return
          case Right(()) => MonixTask.now(())
          case Left(cancelable) =>
            // Blocked on another process to finish the copy of a file, when it's done we restart
            cancelables.synchronized { cancelables.+=(cancelable) }
            MonixTask
              .fromFuture(cancelable)
              .doOnFinish(_ =>
                MonixTask { cancelables.synchronized { cancelables.-=(cancelable) }; () }
              )
        }
    }

    /**
     * Make manual subscription to consumer so that we can control the
     * cancellation for both the source and the consumer. Otherwise, there is
     * no way to call the cancelable produced by the consumer.
     */
    val copyFilesInParallel = Task.create[List[Unit]] { (scheduler, cb) =>
      if (isCancelled.get) {
        cb.onSuccess(Nil)
        subscribed.success(())
        Cancelable.empty
      } else {
        val parallelConsumer =
          Consumer.loadBalance(configuration.parallelUnits, copyFileSequentially)
        val (out, consumerSubscription) = parallelConsumer.createSubscriber(cb, scheduler)
        val cancelOut = Cancelable(() => out.onComplete())
        completeSubscribers = CompositeCancelable(cancelOut)
        val sourceSubscription = observable.subscribe(out)
        subscribed.success(())
        val cancelable = CompositeCancelable(sourceSubscription, consumerSubscription)
        if (!enableCancellation) Cancelable.empty
        else {
          if (isCancelled.get) {
            cancelable.cancel()
          }
          cancelable
        }
      }
    }

    val orderlyDiscovery = Task
      .fromFuture(subscribed.future)
      .flatMap(_ => copyAdditionalFiles)
      .flatMap(_ => discoverFileTree)
    val aggregatedCopyTask = Task {
      Task.mapBoth(orderlyDiscovery, copyFilesInParallel) { case (fileWalk, _) => fileWalk }
    }.flatten.executeOn(scheduler)

    aggregatedCopyTask.doOnCancel(Task {
      if (enableCancellation) {
        isCancelled.compareAndSet(false, true)
        observer.onComplete()
        cancelable.cancel()
      }
    })
  }
}
