package scalajs_java.compiler

import com.sun.tools.javac.tree.JCTree.JCCompilationUnit
import org.scalajs.core.ir.Trees
import org.scalajs.core.tools.logging.{NullLogger, ScalaConsoleLogger}
import org.scalajs.jsenv.ConsoleJSConsole

import scala.collection.JavaConversions._
import scalajs_java.{CompilerInterface, Config, Linker, Runner}
import scalajs_java.compiler.passes._
import scalajs_java.utils.{CompilerPhase, ErrorHandler, Fatal}

/** Aggregates all the passes into a single command */
class CompilerPipeline {

  /** Compiles java CompilationUnit into a list of IR trees */
  def runPasses(compilationUnit: JCCompilationUnit): (List[Trees.ClassDef], Option[String]) = {
        val tree = new JTraversePass(verbose = Config.verbose)
            .run(compilationUnit)

        val opTree = new OpTraversePass(verbose = Config.verbose).run(tree)

        val taggedTree = new RefTagPass(verbose = Config.verbose).run(opTree)

        val fullTree = new EnclClassPass(verbose = Config.verbose).run(taggedTree)

        val ir = new CompilerPass(verbose = Config.verbose).run(fullTree)

        ir
  }

  def run(project: String): Unit = {
    val errorHandler = new ErrorHandler(CompilerPhase("Post-compile"))

    val javaCompiler = new CompilerInterface()
    javaCompiler.compileProject(project)

    val compResults = javaCompiler.compilationUnits.toList.map(runPasses)
    val mainObjects = compResults.map(_._2).collect {
      case Some(name) => name
    }

    if (mainObjects.length > 1)
      errorHandler.fail(0, Some("run"), "Multiple main classes detected", Fatal)
    if (mainObjects.isEmpty)
      errorHandler.fail(0, Some("run"), "No main class detected", Fatal)

    val mainObjectName = mainObjects.head

    val defs = compResults.flatMap(_._1)

    val linked = Linker.link(defs, new ScalaConsoleLogger)

    // Clearly separate the output of the program from the compiling logs
    println("")
    println("")

    Runner.run(mainObjectName, linked, NullLogger, ConsoleJSConsole)
  }

}
