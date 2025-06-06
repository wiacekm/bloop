package bloop.util

import java.io.File
import java.lang.management.ManagementFactory
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.FileTime
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

import scala.concurrent.Await
import scala.concurrent.ExecutionException
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.TimeUnit
import scala.tools.nsc.Properties
import scala.util.control.NonFatal

import bloop.CompilerCache
import bloop.DependencyResolution
import bloop.ScalaInstance
import bloop.bsp.BloopRpcServices
import bloop.cli.Commands
import bloop.config.Config
import bloop.config.Config.CompileOrder
import bloop.config.Tag
import bloop.data.JdkConfig
import bloop.data.LoadedProject
import bloop.data.Origin
import bloop.data.Project
import bloop.data.WorkspaceSettings
import bloop.engine.Action
import bloop.engine.Build
import bloop.engine.BuildLoader
import bloop.engine.ExecutionContext
import bloop.engine.Interpreter
import bloop.engine.Run
import bloop.engine.State
import bloop.engine.caches.ResultsCache
import bloop.engine.caches.SourceGeneratorCache
import bloop.internal.build.BuildTestInfo
import bloop.io.AbsolutePath
import bloop.io.Environment.LineSplitter
import bloop.io.Environment.lineSeparator
import bloop.io.Paths.delete
import bloop.io.RelativePath
import bloop.logging.BloopLogger
import bloop.logging.BspClientLogger
import bloop.logging.BufferedLogger
import bloop.logging.DebugFilter
import bloop.logging.Logger
import bloop.logging.RecordingLogger

import _root_.bloop.task.Task
import _root_.monix.execution.Scheduler
import org.junit.Assert
import sbt.internal.inc.BloopComponentCompiler
import xsbti.ComponentProvider

object TestUtil {
  def projectDir(base: Path, name: String): Path = base.resolve(name)
  def sourcesDir(base: Path, name: String): Path = projectDir(base, name).resolve("src")
  def targetDir(base: Path, name: String): Path = projectDir(base.resolve("target"), name)
  def classesDir(base: Path, name: String): Path = targetDir(base, name).resolve("classes")
  def getBaseFromConfigDir(configDir: Path): Path = configDir.getParent.getParent
  def getProject(name: String, state: State): Project =
    state.build.getProjectFor(name).getOrElse(sys.error(s"Project '$name' does not exist!"))

  val jdkVersion: String = sys.props("java.version")
  def isJdk8: Boolean = jdkVersion.startsWith("8") || jdkVersion.startsWith("1.8")

  def runOnlyOnJava8(thunk: => Unit): Unit = {
    if (isJdk8) thunk
    else ()
  }

  final val componentProvider: ComponentProvider =
    BloopComponentCompiler.getComponentProvider(bloop.io.Paths.getCacheDirectory("components"))

  private var singleCompilerCache: CompilerCache = null
  def getCompilerCache(logger: Logger): CompilerCache = synchronized {
    if (singleCompilerCache != null) singleCompilerCache.withLogger(logger)
    else {
      singleCompilerCache = new CompilerCache(componentProvider, logger)
      singleCompilerCache
    }
  }

  final lazy val scalaInstance: ScalaInstance = {
    ScalaInstance.resolve(
      "org.scala-lang",
      "scala-compiler",
      Properties.versionNumberString,
      bloop.logging.NoopLogger
    )
  }

  final val RootProject = "target-project"
  def checkAfterCleanCompilation(
      structures: Map[String, Map[String, String]],
      dependencies: Map[String, Set[String]],
      rootProjects: List[String] = List(RootProject),
      scalaInstance: ScalaInstance = TestUtil.scalaInstance,
      jdkConfig: JdkConfig = JdkConfig.default,
      quiet: Boolean = false,
      failure: Boolean = false,
      useSiteLogger: Option[Logger] = None,
      order: CompileOrder = Config.Mixed
  )(afterCompile: State => Unit = (_ => ())): Unit = {
    testState(structures, dependencies, scalaInstance, jdkConfig, order) { (state: State) =>
      def action(state0: State): Unit = {
        val state = useSiteLogger.map(logger => state0.copy(logger = logger)).getOrElse(state0)
        // Check that this is a clean compile!
        val projects = state.build.loadedProjects.map(_.project)
        assert(projects.forall(p => noPreviousAnalysis(p, state)))
        val action = Run(Commands.Compile(rootProjects, incremental = true))
        val compiledState = TestUtil.blockingExecute(action, state)
        afterCompile(compiledState)
      }

      val logger = state.logger
      if (quiet) quietIfSuccess(logger)(newLogger => action(state.copy(logger = newLogger)))
      else if (failure) quietIfError(logger)(newLogger => action(state.copy(logger = newLogger)))
      else action(state)
    }
  }

  def await[T](
      duration: Duration,
      scheduler: Scheduler,
      logger: Option[RecordingLogger] = None
  )(t: Task[T]): T = {
    val handle = t.runAsync(scheduler)
    try Await.result(handle, duration)
    catch {
      case NonFatal(t) => handle.cancel(); throw t
      case i: InterruptedException => handle.cancel(); throw i
      case t: TimeoutException =>
        System.err.println("Error: timeout detected, printing logs!")
        logger.foreach(_.dump())
        System.err.println("Now, taking a thread dump!")
        System.err.println(threadDump)
        System.err.println("Rethrowing exception to the caller!")
        throw t
    }
  }

  def await[T](length: Long, unit: TimeUnit)(t: Task[T]): T = {
    await(FiniteDuration(length, unit))(t)
  }

  def await[T](duration: Duration)(t: Task[T]): T = {
    await(duration, ExecutionContext.scheduler)(t)
  }

  def interpreterTask(a: Action, state: State): Task[State] = {
    Interpreter.execute(a, Task.now(state))
  }

  def blockOnTask[T](
      task: Task[T],
      seconds: Long,
      loggers: List[RecordingLogger] = Nil,
      userScheduler: Option[Scheduler] = None
  ): T = {
    val duration = Duration(seconds, TimeUnit.SECONDS)
    val handle = task.runAsync(userScheduler.getOrElse(ExecutionContext.scheduler))
    try Await.result(handle, duration)
    catch {
      case NonFatal(t) =>
        handle.cancel()
        loggers.foreach(logger => { logger.dump(); Thread.sleep(100) })
        t match {
          case e: ExecutionException => throw e.getCause()
          case _ => throw t
        }
    }
  }

  def blockingExecute(a: Action, state: State, duration: Duration = Duration.Inf): State = {
    val handle = interpreterTask(a, state).runAsync(ExecutionContext.scheduler)
    try Await.result(handle, duration)
    catch {
      case NonFatal(t) => handle.cancel(); throw t
      case _: InterruptedException => handle.cancel(); state
    }
  }

  def quietIfError[T](logger: Logger)(op: BufferedLogger => T): T = {
    val bufferedLogger = BufferedLogger(logger.asVerbose)
    try op(bufferedLogger)
    catch {
      case ex: Throwable => bufferedLogger.clear(); throw ex
    }
  }

  def quietIfSuccess[T](logger: Logger)(op: BufferedLogger => T): T = {
    val bufferedLogger = BufferedLogger(logger.asVerbose)
    try op(bufferedLogger)
    catch {
      case ex: Throwable => bufferedLogger.flush(); throw ex
    }
  }

  def getBloopConfigDir(buildName: String): Path = {
    val baseDirURL = ThisClassLoader.getResource(buildName)
    if (baseDirURL == null) {
      sys.error(s"Project ${buildName} does not exist in test resources")
    } else {
      val baseDir = java.nio.file.Paths.get(baseDirURL.toURI)
      val bloopConfigDir = baseDir.resolve("bloop-config")
      if (Files.exists(bloopConfigDir)) bloopConfigDir
      else sys.error(s"Project ${buildName} does not exist in test resources")
    }
  }

  private final val ThisClassLoader = this.getClass.getClassLoader

  def loadTestProject(buildName: String): State = {
    val configDir = getBloopConfigDir(buildName)
    val logger = BloopLogger.default(configDir.toString())
    loadTestProject(configDir, logger, true)
  }

  def loadTestProject(buildName: String, logger: Logger): State =
    loadTestProject(getBloopConfigDir(buildName), logger, true)

  def loadTestProject(configDir: Path): State = {
    val logger = BloopLogger.default(configDir.toString())
    loadTestProject(configDir, logger, false)
  }

  def loadTestProject(configDir: Path, logger: Logger): State =
    loadTestProject(configDir, logger, false, identity[List[Project]] _)

  def loadTestProject(configDir: Path, logger: Logger, emptyResults: Boolean): State =
    loadTestProject(configDir, logger, emptyResults, identity[List[Project]] _)

  def loadTestProject(
      configDir: Path,
      logger: Logger,
      emptyResults: Boolean,
      transformProjects: List[Project] => List[Project]
  ): State = {
    assert(Files.exists(configDir), "Does not exist: " + configDir)

    val configDirectory = AbsolutePath(configDir)
    val loadedProjects = BuildLoader.loadSynchronously(configDirectory, logger)
    val transformedProjects = loadedProjects.map {
      case LoadedProject.RawProject(project) =>
        LoadedProject.RawProject(transformProjects(List(project)).head)
      case LoadedProject.ConfiguredProject(project, original, settings) =>
        LoadedProject.ConfiguredProject(
          transformProjects(List(project)).head,
          original,
          settings
        )
    }
    val workspaceSettings = WorkspaceSettings.readFromFile(configDirectory, logger)
    val build = Build(configDirectory, transformedProjects, workspaceSettings)
    val state =
      State.forTests(build, TestUtil.getCompilerCache(logger), SourceGeneratorCache.empty, logger)
    val state1 = state.copy(commonOptions = state.commonOptions.copy(env = runAndTestProperties))
    if (!emptyResults) state1 else state1.copy(results = ResultsCache.emptyForTests)
  }

  private[bloop] def runAndTestProperties = {
    new bloop.cli.CommonOptions.PrettyProperties(Map("BLOOP_OWNER" -> "owner"))
  }

  /**
   * Compile the given sources and then run `cmd`. Log messages are then given to `check`.
   *
   * @param sources The sources to compile.
   * @param cmd     The command to execute after compiling.
   * @param check   A function that'll receive the resulting log messages.
   */
  def runAndCheck(projectName: String, sources: Seq[String], cmd: Commands.CompilingCommand)(
      check: List[(String, String)] => Unit
  ): Unit = {
    val noDependencies = Map.empty[String, Set[String]]
    val namedSources = sources.zipWithIndex.map { case (src, idx) => s"src$idx.scala" -> src }.toMap
    val projectsStructure = Map(projectName -> namedSources)
    val jdkConfig = JdkConfig.default
    checkAfterCleanCompilation(
      projectsStructure,
      noDependencies,
      rootProjects = List(projectName),
      jdkConfig = jdkConfig,
      quiet = true
    ) { state =>
      runAndCheck(state, cmd)(check)
    }
  }

  /**
   * Executes the given `cmd` on `state`. The resulting log messages are passed to `check`.
   *
   * @param state The current state
   * @param cmd   The command to execute.
   * @param check A function that'll receive the resulting log messages.
   */
  def runAndCheck(state: State, cmd: Commands.CompilingCommand)(
      check: List[(String, String)] => Unit
  ): Unit = {
    val recordingLogger = new RecordingLogger
    val commonOptions = state.commonOptions.copy(env = runAndTestProperties)
    val recordingState = state.copy(logger = recordingLogger).copy(commonOptions = commonOptions)
    TestUtil.blockingExecute(Run(cmd), recordingState)
    try check(recordingLogger.getMessages)
    catch {
      case NonFatal(t) =>
        recordingLogger.dump()
        throw t
    }
  }

  def testState[T](
      projectStructures: Map[String, Map[String, String]],
      dependenciesMap: Map[String, Set[String]],
      instance: ScalaInstance = TestUtil.scalaInstance,
      jdkConfig: JdkConfig = JdkConfig.default,
      order: CompileOrder = Config.Mixed,
      userLogger: Option[Logger] = None,
      extraJars: Array[AbsolutePath] = Array(),
      testProjects: Set[String] = Set.empty
  )(op: State => T): T = {
    withinWorkspace { temp =>
      val logger = userLogger.getOrElse(BloopLogger.default(temp.toString))
      val projects = projectStructures.map {
        case (name, sources) =>
          val instance1 = Some(instance)
          val deps = dependenciesMap.getOrElse(name, Set.empty)
          val tags = if (testProjects.contains(name)) Tag.Test :: Nil else Nil
          makeProject(
            temp,
            name,
            sources,
            deps,
            instance1,
            jdkConfig,
            logger,
            order,
            extraJars,
            tags
          )
      }
      val loaded = projects.map(p => LoadedProject.RawProject(p))
      val build = Build(temp, loaded.toList, None)
      val state =
        State.forTests(build, TestUtil.getCompilerCache(logger), SourceGeneratorCache.empty, logger)
      try op(state)
      catch {
        case NonFatal(t) =>
          userLogger match {
            case Some(logger: RecordingLogger) =>
              System.err.println("Printing logs before rethrowing exception...")
              logger.dump()
            case _ => ()
          }

          throw t
      }
    }
  }

  def noPreviousAnalysis(project: Project, state: State): Boolean =
    !state.results.lastSuccessfulResultOrEmpty(project).previous.analysis().isPresent

  def hasPreviousResult(project: Project, state: State): Boolean =
    state.results.lastSuccessfulResult(project).isDefined

  private[bloop] def syntheticOriginFor(path: AbsolutePath): Origin =
    Origin(path, FileTime.fromMillis(0), 0L, scala.util.Random.nextInt())

  def makeProject(
      baseDir: AbsolutePath,
      name: String,
      sources: Map[String, String],
      dependencies: Set[String],
      scalaInstance: Option[ScalaInstance],
      jdkConfig: JdkConfig,
      logger: Logger,
      compileOrder: CompileOrder,
      extraJars: Array[AbsolutePath],
      tags: List[String]
  ): Project = {
    val origin = syntheticOriginFor(baseDir)
    val baseDirectory = projectDir(baseDir.underlying, name)
    val ProjectArchetype(srcs, out, _, classes, _) =
      createProjectArchetype(baseDir.underlying, name)
    val tempDir = baseDirectory.resolve("tmp")
    Files.createDirectories(tempDir)
    Files.createDirectories(classes.underlying)

    // Requires dependencies to be transitively listed
    val depsTargets =
      (dependencies.map(classesDir(baseDir.underlying, _))).map(AbsolutePath.apply).toList
    val allJars = scalaInstance.map(_.allJars.map(AbsolutePath.apply)).getOrElse(Array.empty)
    val classpath = depsTargets ++ allJars ++ extraJars
    val sourceDirectories = List(srcs)
    val testFrameworks =
      if (classpath.exists(_.syntax.contains("junit"))) List(Config.TestFramework.JUnit) else Nil

    writeFilesToBase(srcs, sources.map(kv => RelativePath(kv._1) -> kv._2))
    Project(
      name = name,
      baseDirectory = AbsolutePath(baseDirectory),
      workspaceDirectory = Option(baseDir),
      dependencies = dependencies.toList,
      scalaInstance = scalaInstance,
      rawClasspath = classpath,
      resources = Nil,
      isBestEffort = false,
      compileSetup = Config.CompileSetup.empty.copy(order = compileOrder),
      genericClassesDir = classes,
      scalacOptions = Nil,
      javacOptions = Nil,
      sources = sourceDirectories,
      sourcesGlobs = Nil,
      sourceRoots = None,
      sourceGenerators = Nil,
      testFrameworks = testFrameworks,
      testOptions = Config.TestOptions.empty,
      out = out,
      analysisOut = out.resolve(Config.Project.analysisFileName(name)),
      platform = Project.defaultPlatform(logger, classpath, Nil, Some(jdkConfig)),
      sbt = None,
      resolution = None,
      tags = tags,
      origin = origin
    )
  }

  case class ProjectArchetype(
      sourceDir: AbsolutePath,
      targetDir: AbsolutePath,
      resourcesDir: AbsolutePath,
      classesDir: AbsolutePath,
      runtimeResourcesDir: AbsolutePath
  )

  def createProjectArchetype(base: Path, name: String): ProjectArchetype = {
    val sourceDir = sourcesDir(base, name)
    val target = targetDir(base, name)
    val resourcesDir = base.resolve("resources")
    val classes = classesDir(base, name)
    val runtimeResources = base.resolve("runtime-resources")
    Files.createDirectories(sourceDir)
    Files.createDirectories(target)
    Files.createDirectories(resourcesDir)
    Files.createDirectories(classes)
    Files.createDirectories(runtimeResources)
    ProjectArchetype(
      AbsolutePath(sourceDir),
      AbsolutePath(target),
      AbsolutePath(resourcesDir),
      AbsolutePath(classes),
      AbsolutePath(runtimeResources)
    )
  }

  def ensureCompilationInAllTheBuild(state: State): Unit = {
    state.build.loadedProjects.foreach { loadedProject =>
      val p = loadedProject.project
      Assert.assertTrue(s"${p.name} was not compiled", hasPreviousResult(p, state))
    }
  }

  def writeFilesToBase(base: AbsolutePath, pathsToContents: Map[RelativePath, String]): Unit = {
    pathsToContents.foreach {
      case (relativePath, contents) =>
        val fullPath = relativePath.toAbsolute(base)
        Files.createDirectories(fullPath.getParent.underlying)
        Files.write(fullPath.underlying, contents.getBytes("UTF-8"))
    }
  }

  /** Creates an empty workspace where operations can happen. */
  def withinWorkspace[T](op: AbsolutePath => T): T = {
    val temp = Files.createTempDirectory("bloop-test-workspace").toRealPath()
    try op(AbsolutePath(temp))
    finally delete(AbsolutePath(temp))
  }

  /** Creates an empty workspace where operations can happen. */
  def withinWorkspace[T](op: AbsolutePath => Task[T]): Task[T] = {
    val temp = Files.createTempDirectory("bloop-test-workspace").toRealPath()
    op(AbsolutePath(temp)).doOnFinish(_ => Task(delete(AbsolutePath(temp))))
  }

  def withTemporaryFile[T](op: Path => T): T = {
    val temp = Files.createTempFile("tmp", "")
    try op(temp)
    finally delete(AbsolutePath(temp))
  }

  def errorsFromLogger(logger: RecordingLogger): List[String] =
    logger.getMessages.iterator.filter(_._1 == "error").map(_._2).toList

  /** Fails the test with a pretty diff if there obtained is not the same as expected */
  def assertNoDiff(expected: String, obtained: String): Unit = {
    import scala.collection.JavaConverters._
    if (obtained.isEmpty && !expected.isEmpty) Assert.fail("obtained empty output")
    def splitLines(string: String): java.util.List[String] =
      string.trim.replace("\r\n", "\n").split("\n").toSeq.asJava
    val obtainedLines = splitLines(obtained)
    val expectedLines = splitLines(expected)
    val patch = difflib.DiffUtils.diff(expectedLines, obtainedLines)
    val diff =
      if (patch.getDeltas.isEmpty) ""
      else {
        difflib.DiffUtils
          .generateUnifiedDiff(
            "expected",
            "obtained",
            expectedLines,
            patch,
            1
          )
          .asScala
          .mkString("\n")
      }
    if (!diff.isEmpty) {
      Assert.fail("\n" + diff)
    }
  }

  def universalPath(path: String): String = {
    path.split('/').mkString(File.separator)
  }

  def createSimpleRecursiveBuild(bloopDir: RelativePath): AbsolutePath = {
    import bloop.config.Config
    val baseDir = Files.createTempDirectory("bloop-recursive-project")
    baseDir.toFile.deleteOnExit()
    val configDir = AbsolutePath(baseDir).resolve(bloopDir)
    Files.createDirectory(configDir.underlying)
    val jsonTargetG = configDir.resolve("g.json").underlying
    val outDir = Files.createDirectory(baseDir.resolve("out"))
    val classesDir = Files.createDirectory(outDir.resolve("classes"))

    // format: OFF
    val configFileG = bloop.config.Config.File(Config.File.LatestVersion, Config.Project("g", baseDir, Option(baseDir), Nil, None, None, List("g"), Nil, outDir, classesDir, None, None, None, None, None, None, None, None, None))
    bloop.config.write(configFileG, jsonTargetG)
    // format: ON

    configDir
  }

  def createTestServices(
      addDiagnosticsHandler: Boolean,
      logger0: BspClientLogger[_]
  ): BloopRpcServices = {
    implicit val ctx: DebugFilter = DebugFilter.Bsp
    import ch.epfl.scala.bsp
    import ch.epfl.scala.bsp.endpoints
    val logger: bloop.logging.Logger = logger0
    def fmt(msg: String, originId: Option[String]): String = {
      originId match {
        case None => msg
        case Some(origin) => msg + s"(origin id: $origin)"
      }
    }

    val rawServices = BloopRpcServices
      .empty(logger0)
      .notification(endpoints.Build.showMessage) {
        case bsp.ShowMessageParams(bsp.MessageType.Log, _, o, msg) => logger.debug(fmt(msg, o))
        case bsp.ShowMessageParams(bsp.MessageType.Info, _, o, msg) => logger.info(fmt(msg, o))
        case bsp.ShowMessageParams(bsp.MessageType.Warning, _, o, msg) => logger.warn(fmt(msg, o))
        case bsp.ShowMessageParams(bsp.MessageType.Error, _, o, msg) => logger.error(fmt(msg, o))
      }
      .notification(endpoints.Build.logMessage) {
        case bsp.LogMessageParams(bsp.MessageType.Log, _, o, msg) => logger.debug(fmt(msg, o))
        case bsp.LogMessageParams(bsp.MessageType.Info, _, o, msg) => logger.info(fmt(msg, o))
        case bsp.LogMessageParams(bsp.MessageType.Warning, _, o, msg) => logger.warn(fmt(msg, o))
        case bsp.LogMessageParams(bsp.MessageType.Error, _, o, msg) => logger.error(fmt(msg, o))
      }

    // Lsp4s fails if we try to repeat a handler for a given notification
    if (!addDiagnosticsHandler) rawServices
    else {
      rawServices.notification(endpoints.Build.publishDiagnostics) {
        case bsp.PublishDiagnosticsParams(_, _, originId, diagnostics, _) =>
          // We prepend diagnostics so that tests can check they came from this notification
          def printDiagnostic(d: bsp.Diagnostic): String =
            fmt(s"[diagnostic] ${d.message} ${d.range}", originId)
          diagnostics.foreach { d =>
            d.severity match {
              case Some(bsp.DiagnosticSeverity.Error) => logger.error(printDiagnostic(d))
              case Some(bsp.DiagnosticSeverity.Warning) => logger.warn(printDiagnostic(d))
              case Some(bsp.DiagnosticSeverity.Information) => logger.info(printDiagnostic(d))
              case Some(bsp.DiagnosticSeverity.Hint) => logger.debug(printDiagnostic(d))
              case None => logger.info(printDiagnostic(d))
            }
          }
      }
    }
  }

  case class ParsedFile(relativePath: RelativePath, contents: String)
  def parseFile(contents0: String): ParsedFile = {
    val contents = contents0.trim
    contents.splitLines match {
      case Array() =>
        sys.error(
          s"""Expected parsed file format:
             |
             |```
             |/relative/path/file.txt
             |contents of txt
             |file
             |```
             |
             |Obtained ${contents}
        """.stripMargin
        )
      case lines =>
        val potentialPath = lines.head
        if (!potentialPath.startsWith("/")) {
          sys.error("First line of contents file does not start with `/`")
        } else {
          val contents = lines.tail.mkString(lineSeparator)
          val relPath = potentialPath.replace("/", File.separator).stripPrefix(File.separator)
          ParsedFile(RelativePath(relPath), contents)
        }
    }
  }

  def loadStateFromProjects(baseDir: AbsolutePath, projects: List[TestProject]): State = {
    val configDir = TestProject.populateWorkspace(baseDir, projects)
    val logger = BloopLogger.default(configDir.toString())
    TestUtil.loadTestProject(configDir.underlying, logger, false, identity(_))
  }

  def threadDump: String = {
    val sb = new StringBuilder
    val mxBean = ManagementFactory.getThreadMXBean()
    val stacktraces = mxBean.dumpAllThreads(true, true)
    stacktraces.foreach(threadInfo => sb.append(threadInfo.toString()).append("\n"))
    sb.result()
  }

  private def hasPythonNamed(executable: String) = try {
    scala.sys.process.Process(Seq(executable, "--version")).! == 0
  } catch {
    case NonFatal(_) => false
  }

  private lazy val hasPython3 = hasPythonNamed("python3")
  private lazy val hasPython2 = hasPythonNamed("python")

  lazy val generator: List[String] =
    if (hasPython3) List("python3", BuildTestInfo.sampleSourceGenerator.getAbsolutePath)
    else if (hasPython2) List("python", BuildTestInfo.sampleSourceGenerator.getAbsolutePath)
    else Nil

  def getTestNGDep(logger: Logger) = {
    val testNG =
      DependencyResolution.Artifact("org.testng", "testng", "7.9.0")
    DependencyResolution.resolve(List(testNG), logger)
  }
}
