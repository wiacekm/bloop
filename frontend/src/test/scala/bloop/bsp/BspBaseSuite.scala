package bloop.bsp

import java.net.BindException
import java.net.URI
import java.nio.file.Files
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

import ch.epfl.scala.bsp
import ch.epfl.scala.bsp.ScalacOptionsResult
import ch.epfl.scala.bsp.endpoints

import bloop.TestSchedulers
import bloop.bsp.BloopBspDefinitions.BloopExtraBuildParams
import bloop.cli.BspProtocol
import bloop.cli.Commands
import bloop.cli.ExitStatus
import bloop.dap.DebugTestClient
import bloop.engine.ExecutionContext
import bloop.engine.State
import bloop.internal.build.BuildInfo
import bloop.io.AbsolutePath
import bloop.io.RelativePath
import bloop.logging.BspClientLogger
import bloop.logging.Logger
import bloop.logging.RecordingLogger
import bloop.task.Task
import bloop.testing.BaseSuite
import bloop.util.CrossPlatform
import bloop.util.TestProject
import bloop.util.TestUtil

import com.github.plokhotnyuk.jsoniter_scala.core._
import jsonrpc4s._
import monix.execution.CancelableFuture
import monix.execution.Scheduler
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable
import monix.reactive.subjects.BehaviorSubject

abstract class BspBaseSuite extends BaseSuite with BspClientTest {
  final class UnmanagedBspTestState(
      state: State,
      closeServer: Task[Unit],
      closeStreamsForcibly: () => Unit,
      currentCompileIteration: AtomicInt,
      diagnostics: ConcurrentHashMap[bsp.BuildTargetIdentifier, StringBuilder],
      implicit val client: BloopLanguageClient,
      private val serverStates: Observable[State]
  ) {
    val status = state.status
    def toUnsafeManagedState: ManagedBspTestState = {
      new ManagedBspTestState(
        state,
        bsp.StatusCode.Ok,
        currentCompileIteration,
        diagnostics,
        client,
        serverStates
      )
    }

    def withinSession(f: ManagedBspTestState => Unit): Unit = {
      try f(toUnsafeManagedState)
      finally TestUtil.await(FiniteDuration(1, "s"))(closeServer)
    }

    def withinSession(f: ManagedBspTestState => Task[Unit]): Task[Unit] = {
      f(toUnsafeManagedState)
        .doOnFinish(_ => closeServer)
        .doOnCancel(closeServer)
    }

    def simulateClientDroppingOut(): Unit = closeStreamsForcibly()
  }

  final case class ManagedBspTestState(
      state: State,
      lastBspStatus: bsp.StatusCode,
      currentCompileIteration: AtomicInt,
      val diagnostics: ConcurrentHashMap[bsp.BuildTargetIdentifier, StringBuilder],
      implicit val client0: BloopLanguageClient,
      val serverStates: Observable[State]
  ) {
    val underlying = state
    val client = state.client
    val status = state.status
    val results = state.results

    import endpoints.{BuildTarget, Workspace}

    def rpcRequest[A, B](
        endpoint: Endpoint[A, B],
        params: A
    ): Task[B] = rpcRequest(endpoint, Some(params))

    def rpcRequest[A, B](
        endpoint: Endpoint[A, B],
        params: Option[A]
    ): Task[B] = {
      client0.request(endpoint, params, Map.empty).map {
        case RpcFailure(_, e) => fail(s"The request ${endpoint.method} failed with $e")
        case RpcSuccess(a, _) => a
      }
    }

    def findBuildTarget(project: TestProject): bsp.BuildTarget = {
      val workspaceTargetTask = {
        rpcRequest(Workspace.buildTargets, None).map { ts =>
          ts.targets.map(t => t.id -> t).find(_._1 == project.bspId) match {
            case Some((_, target)) => target
            case None => fail(s"Target ${project.bspId} is missing in the workspace! Found ${ts}")
          }
        }
      }

      TestUtil.await(FiniteDuration(5, "s"))(workspaceTargetTask)
    }

    def workspaceTargets: bsp.WorkspaceBuildTargetsResult = {
      val workspaceTargetsTask =
        rpcRequest(Workspace.buildTargets, None)

      TestUtil.await(FiniteDuration(5, "s"))(workspaceTargetsTask)
    }

    def runAfterTargets[T](
        project: TestProject
    )(f: bsp.BuildTargetIdentifier => Task[T]): Task[T] = {
      rpcRequest(Workspace.buildTargets, None).flatMap { ts =>
        ts.targets.map(_.id).find(_ == project.bspId) match {
          case Some(target) => f(target)
          case None => fail(s"Target ${project.bspId} is missing in the workspace! Found ${ts}")
        }
      }
    }

    def compileTask(
        project: TestProject,
        originId: Option[String],
        clearDiagnostics: Boolean = true,
        arguments: Option[List[String]] = None
    ): Task[ManagedBspTestState] = {
      runAfterTargets(project) { target =>
        // Handle internal state before sending compile request
        if (clearDiagnostics) diagnostics.clear()
        currentCompileIteration.increment(1)

        rpcRequest(BuildTarget.compile, bsp.CompileParams(List(target), originId, arguments))
          .flatMap { r =>
            // `headL` returns latest saved state from bsp because source is behavior subject
            Task
              .liftMonixTaskUncancellable(
                serverStates.headL
              )
              .map { state =>
                new ManagedBspTestState(
                  state,
                  r.statusCode,
                  currentCompileIteration,
                  diagnostics,
                  client0,
                  serverStates
                )
              }
          }
      }
    }

    def compileHandle(
        project: TestProject,
        delay: Option[FiniteDuration] = None,
        userScheduler: Option[Scheduler] = None
    ): CancelableFuture[ManagedBspTestState] = {
      val interpretedTask = {
        val task = compileTask(project, None)
        delay match {
          case Some(duration) => task.delayExecution(duration)
          case None => task
        }
      }

      interpretedTask.runAsync(userScheduler.getOrElse(ExecutionContext.scheduler))
    }

    def compile(
        project: TestProject,
        originId: Option[String] = None,
        clearDiagnostics: Boolean = true,
        timeout: Long = 30,
        arguments: Option[List[String]] = None
    ): ManagedBspTestState = {
      // Use a default timeout of 30 seconds for every operation
      TestUtil.await(FiniteDuration(timeout, "s")) {
        compileTask(project, originId, clearDiagnostics, arguments)
      }
    }

    def testTask(
        project: TestProject,
        originId: Option[String] = None,
        arguments: Option[List[String]] = None,
        dataKind: Option[String] = None,
        data: Option[RawJson] = None,
        clearDiagnostics: Boolean = true
    ): Task[ManagedBspTestState] = {
      runAfterTargets(project) { target =>
        // Handle internal state before sending test request
        if (clearDiagnostics) diagnostics.clear()
        currentCompileIteration.increment(1)
        val params = bsp.TestParams(List(target), originId, arguments, dataKind, data)

        rpcRequest(BuildTarget.test, params).flatMap { r =>
          // `headL` returns latest saved state from bsp because source is behavior subject
          Task
            .liftMonixTaskUncancellable(
              serverStates.headL
            )
            .map { state =>
              new ManagedBspTestState(
                state,
                r.statusCode,
                currentCompileIteration,
                diagnostics,
                client0,
                serverStates
              )
            }
        }
      }
    }

    def test(
        project: TestProject,
        originId: Option[String] = None,
        arguments: Option[List[String]] = None,
        dataKind: Option[String] = None,
        data: Option[RawJson] = None,
        clearDiagnostics: Boolean = true,
        timeout: Long = 5
    ): ManagedBspTestState = {
      val task = testTask(project, originId, arguments, dataKind, data, clearDiagnostics)

      TestUtil.await(FiniteDuration(timeout, "s"))(task)
    }

    def cleanTask(project: TestProject): Task[ManagedBspTestState] = {
      runAfterTargets(project) { target =>
        rpcRequest(BuildTarget.cleanCache, bsp.CleanCacheParams(List(target))).flatMap { r =>
          // `headL` returns latest saved state from bsp because source is behavior subject
          val statusCode = if (r.cleaned) bsp.StatusCode.Ok else bsp.StatusCode.Error
          Task
            .liftMonixTaskUncancellable(
              serverStates.headL
            )
            .map { state =>
              new ManagedBspTestState(
                state,
                statusCode,
                currentCompileIteration,
                diagnostics,
                client0,
                serverStates
              )
            }
        }
      }
    }

    def clean(project: TestProject): ManagedBspTestState = {
      // Use a default timeout of 5 seconds for every clean operation
      TestUtil.await(FiniteDuration(5, "s")) {
        cleanTask(project)
      }
    }

    def runTask(
        project: TestProject,
        originId: Option[String],
        clearDiagnostics: Boolean = true
    ): Task[ManagedBspTestState] = {
      runAfterTargets(project) { target =>
        // Handle internal state before sending compile request
        if (clearDiagnostics) diagnostics.clear()
        currentCompileIteration.increment(1)

        rpcRequest(BuildTarget.run, bsp.RunParams(target, originId, None, None, None)).flatMap {
          r =>
            // `headL` returns latest saved state from bsp because source is behavior subject
            Task
              .liftMonixTaskUncancellable(
                serverStates.headL
              )
              .map { state =>
                new ManagedBspTestState(
                  state,
                  r.statusCode,
                  currentCompileIteration,
                  diagnostics,
                  client0,
                  serverStates
                )
              }
        }
      }
    }

    def runHandle(
        project: TestProject,
        delay: Option[FiniteDuration] = None,
        userScheduler: Option[Scheduler] = None
    ): CancelableFuture[ManagedBspTestState] = {
      val interpretedTask = {
        val task = runTask(project, None)
        delay match {
          case Some(duration) => task.delayExecution(duration)
          case None => task
        }
      }

      interpretedTask.runAsync(userScheduler.getOrElse(ExecutionContext.scheduler))
    }

    def requestSources(project: TestProject): bsp.SourcesResult = {
      val sourcesTask = {
        client0.request(endpoints.BuildTarget.sources, bsp.SourcesParams(List(project.bspId))).map {
          case RpcFailure(_, error) => fail(s"Received error ${error}")
          case RpcSuccess(sources, _) => sources
        }
      }

      TestUtil.await(FiniteDuration(5, "s"))(sourcesTask)
    }

    def requestResources(project: TestProject): bsp.ResourcesResult = {
      val resourcesTask = {
        client0
          .request(endpoints.BuildTarget.resources, bsp.ResourcesParams(List(project.bspId)))
          .map {
            case RpcFailure(_, error) => fail(s"Received error ${error}")
            case RpcSuccess(resources, _) => resources
          }
      }

      TestUtil.await(FiniteDuration(5, "s"))(resourcesTask)
    }

    def requestOutputPaths(project: TestProject): bsp.OutputPathsResult = {
      val outputPathsTask = {
        client0
          .request(endpoints.BuildTarget.outputPaths, bsp.OutputPathsParams(List(project.bspId)))
          .map {
            case RpcFailure(_, error) => fail(s"Received error ${error}")
            case RpcSuccess(resources, _) => resources
          }
      }

      TestUtil.await(FiniteDuration(5, "s"))(outputPathsTask)
    }

    def requestDependencyModules(project: TestProject): bsp.DependencyModulesResult = {
      val dependencyModulesTask = {
        client0
          .request(
            endpoints.BuildTarget.dependencyModules,
            bsp.DependencyModulesParams(List(project.bspId))
          )
          .map {
            case RpcFailure(_, error) => fail(s"Received error ${error}")
            case RpcSuccess(modules, _) => modules
          }
      }

      TestUtil.await(FiniteDuration(5, "s"))(dependencyModulesTask)
    }

    def requestDependencySources(project: TestProject): bsp.DependencySourcesResult = {
      val dependencySourcesTask = {
        client0
          .request(
            endpoints.BuildTarget.dependencySources,
            bsp.DependencySourcesParams(List(project.bspId))
          )
          .map {
            case RpcFailure(_, error) => fail(s"Received error ${error}")
            case RpcSuccess(sources, _) => sources
          }
      }

      TestUtil.await(FiniteDuration(5, "s"))(dependencySourcesTask)
    }

    def requestInverseSources(document: AbsolutePath): bsp.InverseSourcesResult = {
      val inverseSourcesTask = {
        client0
          .request(
            endpoints.BuildTarget.inverseSources,
            bsp.InverseSourcesParams(bsp.TextDocumentIdentifier(bsp.Uri(document.toBspUri)))
          )
          .map {
            case RpcFailure(_, error) => fail(s"Received error ${error}")
            case RpcSuccess(targets, _) => targets
          }
      }

      TestUtil.await(FiniteDuration(5, "s"))(inverseSourcesTask)
    }

    import bloop.cli.ExitStatus
    def toBspStatus(status: ExitStatus): bsp.StatusCode = {
      status match {
        case ExitStatus.Ok => bsp.StatusCode.Ok
        case _ => bsp.StatusCode.Error
      }
    }

    def mainClasses(project: TestProject): bsp.ScalaMainClassesResult = {
      val task = runAfterTargets(project) { target =>
        val params = bsp.ScalaMainClassesParams(List(target), None)
        client0
          .request(endpoints.BuildTarget.scalaMainClasses, params)
          .map {
            case RpcFailure(_, error) => fail(s"Received error $error")
            case RpcSuccess(result, _) => result
          }
      }

      TestUtil.await(FiniteDuration(5, "s"))(task)
    }

    def testClasses(project: TestProject): bsp.ScalaTestClassesResult = {
      val task = runAfterTargets(project) { target =>
        val params = bsp.ScalaTestClassesParams(List(target), None)
        rpcRequest(endpoints.BuildTarget.scalaTestClasses, params)
      }

      TestUtil.await(FiniteDuration(5, "s"))(task)
    }

    def withDebugSession[A](
        project: TestProject,
        paramsFactory: bsp.BuildTargetIdentifier => bsp.DebugSessionParams
    )(f: DebugTestClient => Task[A]): A = {
      def sessionAddress: Task[bsp.DebugSessionAddress] =
        runAfterTargets(project) { target =>
          val params = paramsFactory(target)
          rpcRequest(endpoints.DebugSession.start, params)
        }

      val session = for {
        address <- sessionAddress
        uri = URI.create(address.uri.value)
        client = DebugTestClient(uri)(defaultScheduler)
        result <- f(client)
      } yield result

      val timeout =
        if (CrossPlatform.isWindows) FiniteDuration(60, TimeUnit.SECONDS)
        else FiniteDuration(30, TimeUnit.SECONDS)

      TestUtil.await(timeout)(session)
    }

    def await[A](task: Task[A]): (ManagedBspTestState, A) = {
      TestUtil.await(FiniteDuration(5, "s")) {
        task.flatMap { result =>
          Task
            .liftMonixTaskUncancellable(
              serverStates.headL
            )
            .map { state =>
              val latestServerState = new ManagedBspTestState(
                state,
                toBspStatus(state.status),
                currentCompileIteration,
                diagnostics,
                client0,
                serverStates
              )

              latestServerState -> result
            }
        }
      }
    }

    def scalaOptions(project: TestProject): (ManagedBspTestState, bsp.ScalacOptionsResult) = {
      val scalacOptionsTask: Task[ScalacOptionsResult] = runAfterTargets(project) { target =>
        rpcRequest(endpoints.BuildTarget.scalacOptions, bsp.ScalacOptionsParams(List(target)))
      }
      await(scalacOptionsTask)
    }

    def javacOptions(project: TestProject): (ManagedBspTestState, bsp.JavacOptionsResult) = {
      val javacOptionsTask = runAfterTargets(project) { target =>
        rpcRequest(endpoints.BuildTarget.javacOptions, bsp.JavacOptionsParams(List(target)))
      }
      await(javacOptionsTask)
    }

    def jvmRunEnvironment(
        project: TestProject,
        originId: Option[String]
    ): (ManagedBspTestState, bsp.JvmRunEnvironmentResult) = {
      val jvmEnvironmentTask = runAfterTargets(project) { target =>
        rpcRequest(
          endpoints.BuildTarget.jvmRunEnvironment,
          bsp.JvmRunEnvironmentParams(List(target), originId)
        )
      }

      awaitForTask(jvmEnvironmentTask)
    }

    def jvmTestEnvironment(
        project: TestProject,
        originId: Option[String]
    ): (ManagedBspTestState, bsp.JvmTestEnvironmentResult) = {
      val jvmEnvironmentTask = runAfterTargets(project) { target =>
        rpcRequest(
          endpoints.BuildTarget.jvmTestEnvironment,
          bsp.JvmTestEnvironmentParams(List(target), originId)
        )
      }

      awaitForTask(jvmEnvironmentTask)
    }

    private def awaitForTask[T](jvmEnvironmentTask: Task[T]): (ManagedBspTestState, T) = {
      TestUtil.await(FiniteDuration(5, "s")) {
        jvmEnvironmentTask.flatMap { result =>
          Task
            .liftMonixTaskUncancellable(
              serverStates.headL
            )
            .map { state =>
              val latestServerState = new ManagedBspTestState(
                state,
                toBspStatus(state.status),
                currentCompileIteration,
                diagnostics,
                client0,
                serverStates
              )

              latestServerState -> result
            }
        }
      }
    }

    def lastDiagnostics(project: TestProject): String = {
      Option(diagnostics.get(project.bspId)).map(_.mkString).getOrElse("")
    }

    def backup: ManagedBspTestState = {
      val newState = this.toTestState.backup.state

      new ManagedBspTestState(
        newState,
        this.lastBspStatus,
        this.currentCompileIteration,
        this.diagnostics,
        this.client0,
        this.serverStates
      )
    }

    def toTestState: TestState = new TestState(state)
    def toTestStateFrom(origin: TestState): TestState = {
      val originState = origin.state
      new TestState(
        state.copy(
          logger = originState.logger,
          client = originState.client,
          pool = originState.pool,
          commonOptions = originState.commonOptions
        )
      )
    }
  }

  def waitUntilStartAndCompile(
      state: TestState,
      project: TestProject,
      compileStart: Promise[Unit],
      logger: Logger
  ): CancelableFuture[TestState] = {
    Task
      .fromFuture(compileStart.future)
      .flatMap(_ => state.withLogger(logger).compileTask(project))
      .runAsync(ExecutionContext.ioScheduler)
  }

  private val bspDefaultScheduler: Scheduler = TestSchedulers.async("bsp-default", threads = 4)

  /** The protocol to use for the inheriting test suite. */
  def protocol: BspProtocol

  override def test(name: String)(fun: => Any): Unit = {
    if (isWindows && protocol == BspProtocol.Local) {
      // https://github.com/scalacenter/bloop/issues/281
      super.ignore(name, "DISABLED")(fun)
    } else {
      super.test(name)(fun)
    }
  }

  def testNonWindows(name: String)(fun: => Any): Unit = {
    if (isWindows) {
      super.ignore(name, "DISABLED")(fun)
    } else {
      super.test(name)(fun)
    }
  }

  def testMac(name: String)(fun: => Any): Unit = {
    if (isMac) {
      super.test(name)(fun)
    } else {
      super.ignore(name, "DISABLED")(fun)
    }
  }

  private final lazy val tempDir = Files.createTempDirectory("temp-sockets")
  tempDir.toFile.deleteOnExit()

  def createBspCommand(configDir: AbsolutePath): Commands.ValidatedBsp = {
    protocol match {
      case BspProtocol.Tcp =>
        val portNumber = 7001 + scala.util.Random.nextInt(40000)
        createTcpBspCommand(configDir, portNumber)
      case BspProtocol.Local => createLocalBspCommand(configDir, tempDir)
    }
  }

  case class ManagedBspTestBuild(state: ManagedBspTestState, projects: List[TestProject]) {
    val rawState = state.underlying
    def projectFor(name: String): TestProject = {
      projects.find(_.config.name == name).get
    }
    def configFileFor(project: TestProject): AbsolutePath = {
      rawState.build.getProjectFor(project.config.name).get.origin.path
    }
  }

  def loadBspBuildFromResources(
      buildName: String,
      workspace: AbsolutePath,
      logger: RecordingLogger,
      bspClientName: String = "test-bloop-client",
      bloopExtraParams: BloopExtraBuildParams = BloopExtraBuildParams.empty
  )(runTest: ManagedBspTestBuild => Unit): Unit = {
    val testBuild = loadBuildFromResources(buildName, workspace, logger)
    val testState = testBuild.state
    val configDir = testState.build.origin
    val bspLogger = new BspClientLogger(logger)
    def bspCommand() = createBspCommand(configDir)
    openBspConnection(
      testState.state,
      bspCommand,
      configDir,
      bspLogger,
      clientName = bspClientName,
      bloopExtraParams = bloopExtraParams
    ).withinSession { bspState =>
      val bspTestBuild = ManagedBspTestBuild(bspState, testBuild.projects)
      runTest(bspTestBuild)
    }
  }

  def loadBspStateAsSbtClient(
      workspace: AbsolutePath,
      projects: List[TestProject],
      logger: RecordingLogger,
      ownsBuildFiles: Boolean = false
  )(runTest: ManagedBspTestState => Unit): Unit = {
    val bloopExtraParams = BloopExtraBuildParams.empty.copy(ownsBuildFiles = Some(ownsBuildFiles))
    loadBspState(workspace, projects, logger, "sbt", bloopExtraParams)(runTest)
  }

  def loadBspState(
      workspace: AbsolutePath,
      projects: List[TestProject],
      logger: RecordingLogger,
      bspClientName: String = "test-bloop-client",
      bloopExtraParams: BloopExtraBuildParams = BloopExtraBuildParams.empty,
      compileStartPromises: Option[mutable.HashMap[bsp.BuildTargetIdentifier, Promise[Unit]]] = None
  )(runTest: ManagedBspTestState => Unit): Unit = {
    val bspLogger = new BspClientLogger(logger)
    val configDir = TestProject.populateWorkspace(workspace, projects)
    def bspCommand() = createBspCommand(configDir)
    val state = TestUtil.loadTestProject(configDir.underlying, logger)
    openBspConnection(
      state,
      bspCommand,
      configDir,
      bspLogger,
      clientName = bspClientName,
      bloopExtraParams = bloopExtraParams,
      compileStartPromises = compileStartPromises
    ).withinSession(runTest(_))
  }

  def loadBspStateWithTask(
      workspace: AbsolutePath,
      projects: List[TestProject],
      logger: RecordingLogger,
      bspClientName: String = "test-bloop-client",
      bloopExtraParams: BloopExtraBuildParams = BloopExtraBuildParams.empty,
      compileStartPromises: Option[mutable.HashMap[bsp.BuildTargetIdentifier, Promise[Unit]]] = None
  )(runTest: ManagedBspTestState => Task[Unit]): Task[Unit] = {
    val bspLogger = new BspClientLogger(logger)
    val configDir = TestProject.populateWorkspace(workspace, projects)
    def bspCommand() = createBspCommand(configDir)
    val state = TestUtil.loadTestProject(configDir.underlying, logger)
    openBspConnection(
      state,
      bspCommand,
      configDir,
      bspLogger,
      clientName = bspClientName,
      bloopExtraParams = bloopExtraParams,
      compileStartPromises = compileStartPromises
    ).withinSession(runTest(_))
  }

  def openBspConnection[T](
      state: State,
      cmd: () => Commands.ValidatedBsp,
      configDirectory: AbsolutePath,
      logger: BspClientLogger[_],
      userIOScheduler: Option[Scheduler] = None,
      userComputationScheduler: Option[Scheduler] = None,
      clientName: String = "test-bloop-client",
      bloopExtraParams: BloopExtraBuildParams = BloopExtraBuildParams.empty,
      compileStartPromises: Option[mutable.HashMap[bsp.BuildTargetIdentifier, Promise[Unit]]] =
        None,
      retry: Int = 3
  ): UnmanagedBspTestState = {
    try {
      openBspConnectionUnsafe(
        state,
        cmd(),
        configDirectory,
        logger,
        userIOScheduler,
        userComputationScheduler,
        clientName,
        bloopExtraParams,
        compileStartPromises
      )
    } catch {
      // in case random number
      case _: BindException if retry > 0 =>
        openBspConnection(
          state,
          cmd,
          configDirectory,
          logger,
          userIOScheduler,
          userComputationScheduler,
          clientName,
          bloopExtraParams,
          compileStartPromises,
          retry - 1
        )
    }
  }

  protected def openBspConnectionUnsafe[T](
      state: State,
      cmd: Commands.ValidatedBsp,
      configDirectory: AbsolutePath,
      logger: BspClientLogger[_],
      userIOScheduler: Option[Scheduler] = None,
      userComputationScheduler: Option[Scheduler] = None,
      clientName: String = "test-bloop-client",
      bloopExtraParams: BloopExtraBuildParams = BloopExtraBuildParams.empty,
      compileStartPromises: Option[mutable.HashMap[bsp.BuildTargetIdentifier, Promise[Unit]]] = None
  ): UnmanagedBspTestState = {
    val compileIteration = AtomicInt(0)
    val readyToConnect = Promise[Unit]()
    val subject = BehaviorSubject[State](state)
    // val subject = ConcurrentSubject.behavior[State](state)(ExecutionContext.ioScheduler)
    val computationScheduler = userComputationScheduler.getOrElse(ExecutionContext.scheduler)
    val ioScheduler = userIOScheduler.getOrElse(bspDefaultScheduler)
    val path = RelativePath(configDirectory.underlying.getFileName)
    val bspServer = BspServer.run(
      cmd,
      state,
      path,
      Some(readyToConnect),
      Some(subject),
      computationScheduler,
      ioScheduler
    )

    val bspServerStarted = bspServer
      .doOnFinish(_ => Task(subject.onComplete()))
      .runAsync(ioScheduler)
    val stringifiedDiagnostics = new ConcurrentHashMap[bsp.BuildTargetIdentifier, StringBuilder]()
    val bspClientExecution = establishClientConnection(cmd).flatMap { socket =>
      val in = socket.getInputStream
      val out = socket.getOutputStream

      def addToStringReport(
          btid: bsp.BuildTargetIdentifier,
          add: StringBuilder => StringBuilder
      ): Unit = {
        val f = (b: StringBuilder) => add(if (b == null) new StringBuilder() else b)
        stringifiedDiagnostics.compute(btid, (_, builder0) => f(builder0))
        ()
      }

      val lsClient = BloopLanguageClient.fromOutputStream(out, logger)
      val messages = LowLevelMessage
        .fromInputStream(in, logger)
        .map(msg => LowLevelMessage.toMsg(msg))

      val addDiagnosticsHandler = addServicesTest(
        configDirectory,
        () => compileIteration.get,
        addToStringReport,
        compileStartPromises
      )

      val services = addDiagnosticsHandler(TestUtil.createTestServices(false, logger))
      val lsServer = new BloopLanguageServer(messages, lsClient, services, ioScheduler, logger)

      lsServer.processMessagesSequentiallyTask.runAsync(ioScheduler)

      val cwd = configDirectory.underlying.getParent
      val additionalData =
        Try(writeToArray[BloopExtraBuildParams](bloopExtraParams)).toOption.map(RawJson(_))
      val initializeServer =
        lsClient.request(
          endpoints.Build.initialize,
          bsp.InitializeBuildParams(
            clientName,
            "1.0.0",
            BuildInfo.bspVersion,
            rootUri = bsp.Uri(cwd.toAbsolutePath.toUri),
            capabilities = bsp.BuildClientCapabilities(List("scala", "java")),
            None,
            additionalData
          )
        )

      val initializedTask = {
        val startedServer = Task.fromFuture(readyToConnect.future)
        (startedServer *> initializeServer)
          .flatMap { _ =>
            Task.fromFuture(
              lsClient.notify(endpoints.Build.initialized, None)
            )
          }
      }

      val closeTask = {
        lsClient.request(endpoints.Build.shutdown, None).flatMap { _ =>
          Task.fromFuture(lsClient.notify(endpoints.Build.exit, None)).map { _ =>
            try {
              socket.close()
            } catch {
              case e: Throwable =>
                logger.warn("Error closing socket", e)
            }
            cleanUpLastResources(cmd)
          }
        }
      }

      // This task closes the streams to simulate a client dropping out,
      // but doesn't properly close the server. This happens on purpose.
      val closeStreamsForcibly = () => {
        socket.close()
      }

      initializedTask.map { _ =>
        (closeTask.memoize, closeStreamsForcibly, lsClient, subject)
      }
    }

    import scala.concurrent.Await
    import scala.concurrent.duration.FiniteDuration
    val bspClient = bspClientExecution.runAsync(ioScheduler)

    try {
      // The timeout for all our bsp tests, no matter what operation they run, is 30s
      val (closeServer, closeStreamsForcibly, client, stateObservable) =
        Await.result(bspClient, FiniteDuration(30, "s"))
      new UnmanagedBspTestState(
        state,
        closeServer,
        closeStreamsForcibly,
        compileIteration,
        stringifiedDiagnostics,
        client,
        stateObservable
      )
    } catch {
      case t: Throwable =>
        bspServerStarted.cancel()
        cleanUpLastResources(cmd)
        t match {
          case e: ExecutionException => throw e.getCause
          case _ => throw t
        }
        throw t
    }
  }

  def assertExitStatus(obtainedState: ManagedBspTestState, expected: ExitStatus): Unit =
    assertExitStatus(obtainedState.toTestState, expected)

  def assertInvalidCompilationState(
      state: ManagedBspTestState,
      projects: List[TestProject],
      existsAnalysisFile: Boolean,
      hasPreviousSuccessful: Boolean,
      hasSameContentsInClassesDir: Boolean
  )(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    assertInvalidCompilationState(
      state.toTestState,
      projects,
      existsAnalysisFile,
      hasPreviousSuccessful,
      hasSameContentsInClassesDir
    )
  }

  def assertEmptyCompilationState(
      state: ManagedBspTestState,
      projects: List[TestProject]
  ): Unit = {
    assertEmptyCompilationState(state.toTestState, projects)
  }

  def assertValidCompilationState(
      state: ManagedBspTestState,
      projects: List[TestProject]
  )(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    assertValidCompilationState(state.toTestState, projects)
  }

  def assertDifferentExternalClassesDirs(
      s1: ManagedBspTestState,
      s2: ManagedBspTestState,
      project: TestProject
  )(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    assertDifferentExternalClassesDirs(s1.toTestState, s2.toTestState, project)
  }

  def assertSameExternalClassesDirs(
      s1: ManagedBspTestState,
      s2: ManagedBspTestState,
      project: TestProject
  )(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    assertSameExternalClassesDirs(s1.toTestState, s2.toTestState, project)
  }

  def assertDifferentExternalClassesDirs(
      s1: ManagedBspTestState,
      s2: ManagedBspTestState,
      projects: List[TestProject]
  )(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    assertDifferentExternalClassesDirs(s1.toTestState, s2.toTestState, projects)
  }

  def assertSameExternalClassesDirs(
      s1: ManagedBspTestState,
      s2: ManagedBspTestState,
      projects: List[TestProject]
  )(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    assertSameExternalClassesDirs(s1.toTestState, s2.toTestState, projects)
  }

  def mapBoth[A1, A2](f1: CancelableFuture[A1], f2: CancelableFuture[A2]): Task[(A1, A2)] = {
    Task.mapBoth(Task.fromFuture(f1), Task.fromFuture(f2))((a1, a2) => a1 -> a2)
  }
}
