//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import codex.model.Def
import java.io.OutputStream
import java.nio.file.{Files, Path, Paths}
import java.util.{Date, EnumSet}
import scaled._
import scaled.util.{BufferBuilder, Close}

import org.gradle.tooling._
import org.gradle.tooling.Failure
import org.gradle.tooling.model._
import org.gradle.tooling.events._
import org.gradle.tooling.events.{ProgressListener, ProgressEvent}
import org.gradle.tooling.events.test._
import org.gradle.tooling.model.idea._
import org.gradle.tooling.model.{GradleProject => GP}

object GradleProject {

  // matches: "(w|e): /foo/bar/baz.kt: (LL, CC): some error message"
  val noteM = Matcher.regexp("""^(.): ([^:]+): \((\d+), (\d+)\): (.*)""")

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("gradle", true, classOf[GradleProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, "build.gradle")) 1 else -1
  }
}

class GradleProject (ps :ProjectSpace, r :Project.Root) extends AbstractFileProject(ps, r) {
  import Project._
  import GradleProject._

  private[this] val conn = new Close.Ref[ProjectConnection](toClose) {
    protected def create = {
      val conn = GradleConnector.newConnector()
      conn.forProjectDirectory(root.path.toFile)
      conn.connect()
    }
    override protected def willClose (ref :ProjectConnection) {
      ref.close()
    }
  }

  private val java = new JavaComponent(this)
  addComponent(classOf[JavaComponent], java)

  private val sourceDirsToModule = SeqBuffer[(Path,String)]
  private val testSourceDirs = SeqBuffer[Path]()

  private def moduleForPath (bpath :Path) :Option[String] = {
    for ((path, module) <- sourceDirsToModule) {
      if (bpath.startsWith(path)) return Some(module)
    }
    None
  }

  override def addToBuffer (buffer :RBuffer) {
    // if this is the root project, check whether we need to reroute to the appropriate module
    // project
    if (r.module.length == 0 && buffer.store.file.isDefined) {
      val modOpt = moduleForPath(buffer.store.file.get)
      if (modOpt.isDefined) {
        val mroot = Project.Root(root.path, modOpt.get)
        val seed = Project.Seed(mroot, "gradle", true, classOf[GradleProject], mroot :: Nil)
        val proj = pspace.projectFromSeed(seed)
        proj.ready.onSuccess { proj => proj.addToBuffer(buffer) }
        return
      }
    }
    super.addToBuffer(buffer)
  }

  private case class Info (rootProj :GP, idea :IdeaProject)

  override protected def computeMeta (oldMeta :Project.Meta) = {
    // add our compiler & tester components
    addComponent(classOf[Compiler], new GradleCompiler)
    addComponent(classOf[Tester], new GradleTester)

    // initialize gradle in a background thread because it's slow and grindy
    pspace.wspace.exec.runAsync {
      Info(conn.get.getModel(classOf[GP]), conn.get.getModel(classOf[IdeaProject]))
    } map { finishInit(oldMeta, _) }
  }

  private def finishInit (oldMeta :Project.Meta, info :Info) = {
    // add dirs to our ignores
    val igns = FileProject.stockIgnores
    // igns += FileProject.ignorePath(info.buildDir, root.path)

    // TEMP: aggregate all modules into one giant blob of project metadata
    var srcDirs = Seq.builder[Path]()
    testSourceDirs.clear()
    var buildDirs = Seq.builder[Path]()

    // go through and map all source directories to their owning module
    info.idea.getModules foreach { module =>
      module.getContentRoots foreach { croot =>
        croot.getSourceDirectories foreach { sdir =>
          sourceDirsToModule += (sdir.getDirectory.toPath -> module.getName) }
        croot.getTestDirectories foreach { sdir =>
          sourceDirsToModule += (sdir.getDirectory.toPath -> module.getName) } // TODO: test tag
      }
    }

    var pname = info.rootProj.getName
    if (root.module.length > 0) pname = s"$pname:${root.module}"

    // if we don't have a module, then find the module whose root matches our root
    val modopt = if (root.module.length == 0) info.idea.getModules find {
      _.getContentRoots exists { _.getRootDirectory.toPath == root.path }}
    // otherwise find the module that matches our name
    else info.idea.getModules find { _.getName == root.module }
    modopt.foreach { module =>
      module.getContentRoots foreach { croot =>
        croot.getSourceDirectories foreach { sdir => srcDirs += sdir.getDirectory.toPath }
        croot.getTestDirectories foreach { sdir => testSourceDirs += sdir.getDirectory.toPath }
        croot.getExcludeDirectories foreach { dir =>
          igns += FileProject.ignorePath(dir.toPath, root.path) }
      }
      var buildDir = module.getCompilerOutput.getOutputDir
      if (buildDir != null) buildDirs += buildDir.toPath
    }

    ignores() = igns

    if (!buildDirs.isEmpty) {
      // init our JavaComponent
      val classesDir = buildDirs.head
      val classpath = buildDirs // TODO: depends!
      java.javaMetaV() = java.javaMetaV().copy(
        classes = buildDirs,
        outputDir = classesDir,
        buildClasspath = classpath,
        execClasspath = classpath
      )
      java.addTesters()
    }

    def gradleId = SrcURL("gradle", s"//$pname")
    def mvnId = {
      val mod = conn.get.getModel(classOf[GradleModuleVersion])
      RepoId("mvn", mod.getGroup, mod.getName, mod.getVersion)
    }

    oldMeta.copy(
      name = pname, // TODO
      ids = try {
        Seq(gradleId, mvnId)
      } catch {
        case ume :UnknownModelException => Seq(gradleId)
      },
      sourceDirs = srcDirs.build()
    )
  }

  private class BufferOutputStream (buffer :Buffer) extends OutputStream {
    private val accum = new Array[Byte](4096)
    private var pos = 0

    override def write (b :Int) {
      accum(pos) = b.toByte
      pos += 1
    }
    override def write (bytes :Array[Byte], off :Int, len :Int) {
      if (pos + len > accum.length) {
        println("Zoiks! Overflow!")
      } else {
        System.arraycopy(bytes, off, accum, pos, len)
        pos += len
      }
    }
    override def flush () {
      var text = new String(accum, 0, pos, "UTF-8")
      pos = 0
      metaSvc.exec.runOnUI { buffer.append(Line.fromText(text)) }
    }
    override def close () {}
  }

  private class GradleCompiler extends Compiler(this) {
    import Compiler._

    override def describeEngine = "gradle"

    protected def compile (buffer :Buffer, file :Option[Path]) :Future[Boolean] = {
      var result = Promise[Boolean]()
      val tasks = if (file.isDefined) Array("classes") else Array("clean", "classes")
      conn.get.newBuild.
        forTasks(tasks :_*).
        setStandardOutput(new BufferOutputStream(buffer)).
        setStandardError(new BufferOutputStream(buffer)).
        run(new ResultHandler[Void] {
          override def onComplete (unused :Void) = result.succeed(true)
          // TODO: the exception that indicates compilation failure is nested four levels deep, but
          // it would be nice to differentiate that from some other weird failure...
          override def onFailure (cause :GradleConnectionException) = result.succeed(false)
        })
      return result
    }

    protected def nextNote (buffer :Buffer, start :Loc) :(Note,Loc) = {
      buffer.findForward(noteM, start) match {
        case Loc.None => NoMoreNotes
        case ploc => try {
          val ekind = noteM.group(1)
          val file = Paths.get(noteM.group(2))
          val eline = noteM.group(3).toInt-1
          val ecol = noteM.group(4).toInt-1
          val errPre = noteM.group(5).trim
          // every line after the path with leading whitespace is part of the message
          val desc = Seq.builder[String]()
          desc += errPre
          var pnext = ploc.nextStart
          // while (pnext < buffer.end && buffer.line(pnext).indexOf(Chars.isWhitespace) == 0) {
          //   desc += buffer.line(pnext).asString
          //   pnext = pnext.nextStart
          // }
          (Compiler.Note(Store(file), Loc(eline, ecol), desc.build(), ekind == "e"), pnext)
        } catch {
          case e :Exception => log.log("Error parsing error buffer", e) ; Compiler.NoMoreNotes
        }
      }
    }
  }

  private class GradleTester extends JavaTester(this) {
    override def testSourceDirs = GradleProject.this.testSourceDirs
    override def describeSelf (bb :BufferBuilder) {
      bb.addSection("Test Sources:")
      testSourceDirs foreach { p => bb.add(p.toString) }
    }

    override def runAllTests (window :Window, interact :Boolean) = {
      val buf = logBuffer
      buf.replace(buf.start, buf.end, Line.fromTextNL(s"Tests started at ${new Date}..."))
      var result = Promise[Boolean]()
      var status = new Array[Int](3)
      var fails = SeqBuffer[Failure]()

      def finish () :Unit = window.exec.runOnUI {
        noteResults(window, interact, status(0), toVisits(fails))
      }

      conn.get.newBuild.
        forTasks("test").
        setStandardOutput(new BufferOutputStream(buf)).
        setStandardError(new BufferOutputStream(buf)).
        addProgressListener(new ProgressListener {
          override def statusChanged (event :ProgressEvent) = event match {
            case finish :TestFinishEvent => finish.getResult match {
              case success :TestSuccessResult => status(0) += 1
              case skipped :TestSkippedResult => status(1) += 1
              case failure :TestFailureResult =>
                status(2) += 1
                finish.getDescriptor match {
                  case jvm :JvmTestOperationDescriptor =>
                    failure.getFailures foreach { fail =>
                      extractFailure(fail.getDescription, jvm.getClassName,
                                     jvm.getMethodName, fails)
                    }
                  case _ => // alas
                }
            }
            case _ => // TODO: ignore?
          }
        }, EnumSet.of(OperationType.TEST)).
        run(new ResultHandler[Void] {
          override def onComplete (unused :Void) = finish()
          override def onFailure (cause :GradleConnectionException) = finish()
        })
      true
    }

    override def runTests (window :Window, interact :Boolean, file :Path, types :SeqV[Def]) = {
      true
    }

    override def runTest (window :Window, file :Path, elem :Def) = {
      Future.failure(new Exception("TODO"))
    }
  }

  // override def testSeed =
  //   if (mod.name == "test") None
  //   else pkg.modules.find(_.name == "test").map(m => {
  //     val troot = Project.Root(m.root, false) // Scaled projects don't use testMode
  //     Project.Seed(troot, m.name, true, getClass, List(troot)) })
  // override def depends = moddeps.flatten.toSeq.flatMap(toId) :+ platformDepend
  // private def platformDepend = Project.PlatformId(Project.JavaPlatform, JDK.thisJDK.majorVersion)

  // override def warnings = super.warnings ++ moddeps.flatten.collect {
  //   case md :Depend.MissingId => s"Missing depend: ${md.id}"
  // }

  // def resourceDir :Path = rootPath.resolve("src/resources")

  // // scaled projects don't have a magical test subproject; tests are in a top-level project
  // // (usually a module named tests or test)
  // override protected def createTester () :Tester = new JUnitTester(this) {
  //   override def testSourceDirs = sourceDirs
  //   override def testOutputDir = outputDir
  //   override def testClasspath = buildClasspath
  // }
}
