package scalajs_java.compiler

import scala.language.postfixOps

import com.sun.tools.javac.tree.JCTree.JCCompilationUnit
import org.scalajs.core.ir.Trees
import org.scalajs.core.tools.logging.{NullLogger, ScalaConsoleLogger}
import org.scalajs.jsenv.ConsoleJSConsole

import scala.collection.JavaConversions._
import scalajs_java.{CompilerInterface, Config, Linker, Runner}
import scalajs_java.compiler.passes._
import scalajs_java.utils.{CompilerPhase, ErrorHandler, Fatal, Scope}

/** Aggregates all the passes into a single command */
class CompilerPipeline(verbose: Boolean=Config.verbose) {

  val errorHandler = new ErrorHandler(CompilerPhase("Post-compile"))

  /** Compiles java CompilationUnit into a list of IR trees */
  def runPasses(compilationUnits: List[JCCompilationUnit]): (List[Trees.ClassDef], String) = {
    val trees = compilationUnits.map { cu =>
      new JTraversePass(verbose).run(cu)
    }

    val opTrees = trees.map { t =>
      new OpTraversePass(verbose).run(t)
    }

    val treesScopes = opTrees.map { ot =>
      val expSymsPass = new ExpSymsPass(verbose)
      expSymsPass.run(ot)
      (ot, expSymsPass.scope)
    } unzip

    val opTrees2 = treesScopes._1
    val scope = Scope.mkScope(treesScopes._2)

    val taggedTrees = opTrees2.map { ot =>
      new RefTagPass(verbose, scope).run(ot)
    }

    val fullTrees = taggedTrees.map { tt =>
      new EnclClassPass(verbose).run(tt)
    }

    val initLists = fullTrees.map { ft =>
      val sip = new StaticInitsPass(verbose)
      sip.run(ft)
      sip.inits
    }

    val constructors = ConstructorPass.mkConstructors(fullTrees.map { ft =>
      val cp = new ConstructorPass(verbose)
      cp.run(ft)
      cp.constructors
    })

    val irs = (fullTrees zip initLists).map { ft =>
      new CompilerPass(ft._2, constructors, verbose).run(ft._1)
    }

    val defsObjNames = irs.unzip

    val mainObjects = defsObjNames._2.collect { case Some(x) => x }
    if (mainObjects.length > 1)
      errorHandler.fail(0, Some("run"), "Multiple main classes detected", Fatal)
    if (mainObjects.isEmpty)
      errorHandler.fail(0, Some("run"), "No main class detected", Fatal)

    (defsObjNames._1.flatten, mainObjects.head)
  }

  def run(project: String): Unit = {

    val javaCompiler = new CompilerInterface()
    javaCompiler.compileProject(project)

    val compResults = runPasses(javaCompiler.compilationUnits.toList)
    val defs = compResults._1
    val mainObjectName = compResults._2

    val linked = Linker.link(defs, new ScalaConsoleLogger)

    // Clearly separate the output of the program from the compiling logs
    println("")
    println("")

    Runner.run(mainObjectName, linked, NullLogger, ConsoleJSConsole)
  }

}
