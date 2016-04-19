//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.util.{BufferBuilder, Close}

import org.gradle.tooling._
import org.gradle.tooling.model._
import org.gradle.tooling.model.{GradleProject => GP}

class GradleProject (val root :Project.Root, ps :ProjectSpace) extends AbstractFileProject(ps) {
  import Project._

  private[this] val _conn = new Close.Ref[ProjectConnection](toClose) {
    protected def create = {
      val conn = GradleConnector.newConnector()
      conn.forProjectDirectory(root.path.toFile)
      conn.connect()
    }
    override protected def willClose (ref :ProjectConnection) {
      ref.close()
    }
  }

  private case class Info (rootProj :GP, proj :GP) {
    def name = proj.getName
    lazy val idName = {
      "TODO"
    }
    lazy val ids = try {
      Seq(gradleId, mvnId)
    } catch {
      case ume :UnknownModelException => Seq(gradleId)
    }
    def buildDir = proj.getBuildDirectory.toPath
    private def gradleId = SrcURL("gradle", s"//${rootProj.getName}${proj.getPath}")
    private def mvnId = {
      val mod = _conn.get.getModel(classOf[GradleModuleVersion])
      RepoId("mvn", mod.getGroup, mod.getName, mod.getVersion)
    }
  }

  private[this] val _info = new Close.Ref[Info](toClose) {
    protected def create = {
      val rootProj = _conn.get.getModel(classOf[GP])
      // Gradle returns the root project even if we ask for a connector in a subproject, so find
      // our way back down to the subproject that represents us
      def findProject (proj :GP) :GP = {
        val proot = proj.getProjectDirectory.toPath
        if (root.path == proot) proj
        else if (root.path.startsWith(proot)) {
          val name = proot.relativize(root.path).getName(0).toString
          for (child <- proj.getChildren) if (child.getName == name) return findProject(child)
          proj
        } else proj
      }
      Info(rootProj, findProject(rootProj))
    }
  }

  // // hibernate if build.gradle changes, which will trigger reload
  // { val watchSvc = metaSvc.service[WatchService]
  //   watchSvc.watchFile(buildFile, file => hibernate())
  // }
  // // note that we don't 'close' our watches, we'll keep them active for the lifetime of the editor
  // // because it's low overhead; I may change my mind on this front later, hence this note

  override def name = _info.get.name
  override def idName = _info.get.idName
  override def ids = _info.get.ids

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

  override protected def ignores = FileProject.stockIgnores ++
    Seq(FileProject.ignorePath(_info.get.buildDir))

  // override protected def createCompiler () = {
  //   val ssum = summarizeSources

  //   // TODO: do we want to try to support multi-lingual projects? that sounds like a giant PITA,
  //   // but we could probably at least generate a warning if we have some crazy mishmash of sources

  //   // TEMP: if we have any Kotlin files, we just use the KotlinCompiler
  //   if (ssum.contains("kt")) new KotlinCompiler(this) {
  //     // TODO: this is a lot of annoying duplication...
  //     override def sourceDirs = ScaledProject.this.sourceDirs
  //     override def buildClasspath = ScaledProject.this.buildClasspath
  //     override def outputDir = ScaledProject.this.outputDir

  //     override def javacOpts = pkg.jcopts.toSeq
  //     override def kotlincOpts = ScaledProject.this.kotlincOpts
  //     // override def kotlincVers = ScaledProject.this.kotlincVers

  //     override protected def willCompile () {
  //       if (Files.exists(resourceDir)) Filez.copyAll(resourceDir, outputDir)
  //     }

  //   } else new ScalaCompiler(this) {
  //     override def sourceDirs = ScaledProject.this.sourceDirs
  //     override def buildClasspath = ScaledProject.this.buildClasspath
  //     override def outputDir = ScaledProject.this.outputDir

  //     override def javacOpts = pkg.jcopts.toSeq
  //     override def scalacOpts = ScaledProject.this.scalacOpts
  //     override def scalacVers = ScaledProject.this.scalacVers

  //     override protected def willCompile () {
  //       if (Files.exists(resourceDir)) Filez.copyAll(resourceDir, outputDir)
  //     }
  //   }
  // }

  // // scaled projects don't have a magical test subproject; tests are in a top-level project
  // // (usually a module named tests or test)
  // override protected def createTester () :Tester = new JUnitTester(this) {
  //   override def testSourceDirs = sourceDirs
  //   override def testOutputDir = outputDir
  //   override def testClasspath = buildClasspath
  // }
}

object GradleProject {

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("gradle", true, classOf[GradleProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, "build.gradle")) 1 else -1
  }
}
