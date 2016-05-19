import org.scalajs.core.ir

import ir.Printers._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.jsenv._

import trees.Tree


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
    val treeVisitor = new JTreeVisitor(false)
    compiler.compilationUnit.getTree.accept(treeVisitor)
    println("\n\n")

    println("---------------------------- ENV -------------------------")
    compiler.printEnvs()
  }

  private def compileAndRun(tree: Tree): Unit = {
    val classDef = Compiler.compileMainClass(tree)

    val writer = new java.io.PrintWriter(System.out)
    try {
      val printer = new IRTreePrinter(writer)
      printer.printTopLevelTree(classDef)
    } finally {
      writer.flush()
    }

    val linked = Linker.link(classDef, new ScalaConsoleLogger)

    // Clearly separate the output of the program from the compiling logs
    println("")
    println("")

    Runner.run(linked, NullLogger, ConsoleJSConsole)
  }

}
