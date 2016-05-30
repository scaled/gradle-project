//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import java.io.OutputStream
import java.nio.file.{Files, Path, Paths}
import scaled._
import scaled.util.{BufferBuilder, Close}

import org.gradle.tooling._
import org.gradle.tooling.model._
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

  private case class Info (rootProj :GP, proj :GP, idea :IdeaProject) {
    def name = proj.getName
    lazy val ids = try {
      Seq(gradleId, mvnId)
    } catch {
      case ume :UnknownModelException => Seq(gradleId)
    }
    def buildDir = proj.getBuildDirectory.toPath
    private def gradleId = SrcURL("gradle", s"//${rootProj.getName}${proj.getPath}")
    private def mvnId = {
      val mod = conn.get.getModel(classOf[GradleModuleVersion])
      RepoId("mvn", mod.getGroup, mod.getName, mod.getVersion)
    }
  }

  override def init () {
    // add our compiler component
    addComponent(classOf[Compiler], new GradleCompiler)

    // initialize gradle in a background thread because it's slow and grindy
    pspace.wspace.exec.runAsync {
      val rootProj = conn.get.getModel(classOf[GP])
      // Gradle returns the root project even if we ask for a connector in a subproject, so find
      // our way back down to the subproject that represents us
      def findProject (proj :GP) :GP = {
        val proot = proj.getProjectDirectory.toPath
        println("Checking " + proot + " / " + root.path)
        if (root.path == proot) proj
        else if (root.path.startsWith(proot)) {
          val name = proot.relativize(root.path).getName(0).toString
          for (child <- proj.getChildren) if (child.getName == name) return findProject(child)
          proj
        } else proj
      }
      Info(rootProj, findProject(rootProj), conn.get.getModel(classOf[IdeaProject]))
    } onSuccess { finishInit }
  }

  private def finishInit (info :Info) {
    // init our JavaComponent
    val classesDir = info.buildDir
    val classpath = Seq(classesDir) // TODO
    java.javaMetaV() = java.javaMetaV().copy(
      classes = Seq(classesDir),
      outputDir = classesDir,
      buildClasspath = classpath,
      execClasspath = classpath
    )
    java.addTesters()

    // add dirs to our ignores
    val igns = FileProject.stockIgnores
    igns += FileProject.ignorePath(info.buildDir, root.path)

    // TEMP: aggregate all modules into one giant blob of project metadata
    var srcDirs = Seq.builder[Path]()
    var buildDirs = Seq.builder[Path]()
    info.idea.getModules foreach { module =>
      module.getContentRoots foreach { croot =>
        croot.getSourceDirectories foreach { sdir => srcDirs += sdir.getDirectory.toPath }
        croot.getExcludeDirectories foreach { dir =>
          igns += FileProject.ignorePath(dir.toPath, root.path) }
      }
      var buildDir = module.getCompilerOutput.getOutputDir
      if (buildDir != null) buildDirs += buildDir.toPath
    }

    ignores() = igns

    metaV() = metaV().copy(
      name = info.name,
      ids = info.ids,
      sourceDirs = srcDirs.build()
    )
  }

  private class GradleCompiler extends Compiler(this) {
    import Compiler._

    override def describeEngine = "gradle"

    protected def compile (buffer :Buffer, file :Option[Path]) :Future[Boolean] = {
      class BufferOutputStream extends OutputStream {
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

      var result = Promise[Boolean]()
      val tasks = if (file.isDefined) Array("classes") else Array("clean", "classes")
      conn.get.newBuild.
        forTasks(tasks :_*).
        setStandardOutput(new BufferOutputStream()).
        setStandardError(new BufferOutputStream()).
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
