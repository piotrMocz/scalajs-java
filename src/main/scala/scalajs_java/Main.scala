package scalajs_java

import org.scalajs.core.ir
import org.scalajs.core.ir.Trees.ClassDef
import ir.Printers._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.jsenv._
import scalajs_java.trees.{CompilationUnit, Tree}


object Main {

  def main(args: Array[String]): Unit = {
    val compiler = new CompilerInterface()
    compiler.compile(Config.testFilePath)

    println(compiler.compilationUnit.getImports)
    println(compiler.compilationUnit.getPackageName)


    println("------------------------- Scala AST ----------------------")
    val tree = TreeTraverse.traverse(compiler.compilationUnit)
    println(tree.toString)
    println("\n\n")

    println("---------------------------- AST -------------------------")
    val treeVisitor = new JTreeVisitor(true)
    compiler.compilationUnit.getTree.accept(treeVisitor)
    println("\n\n")

    println("---------------------------- ENV -------------------------")
    compiler.printEnvs()

    println()
    println("---------------------------- IR  -------------------------")
    val ir = Compiler.compile(tree)
    println(ir.toString)

    println()
    println("------------------------ Running -------------------------")
    compileAndRun(tree)

  }

  private def compileAndRun(compilationUnit: CompilationUnit): Unit = {
    val defs = Compiler.compile(compilationUnit)

    val writer = new java.io.PrintWriter(System.out)
    try {
      val printer = new IRTreePrinter(writer)
      defs foreach { d =>
        printer.printTopLevelTree(d)
      }
    } finally {
      writer.flush()
    }

    // TODO compile more classes in one file
    val linked = Linker.link(defs, new ScalaConsoleLogger)

    // Clearly separate the output of the program from the compiling logs
    println("")
    println("")

    Runner.run(linked, NullLogger, ConsoleJSConsole)
  }

}
