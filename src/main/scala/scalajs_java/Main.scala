package scalajs_java

import org.scalajs.core.ir
import org.scalajs.core.ir.Trees.ClassDef
import ir.Printers._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.jsenv._

import scalajs_java.compiler.Compiler
import scalajs_java.traversals.{JTreeTraverse, ScopedTraverse, Traverse}
import scalajs_java.trees._

object Main {

  def main(args: Array[String]): Unit = {
    val javaCompiler = new CompilerInterface()
    javaCompiler.compile(Config.testFilePath)

    println("------------------------- Scala AST ----------------------")
    val tree = JTreeTraverse.traverse(javaCompiler.compilationUnit)
    println(tree.toString)
    println("\n\n")

    println("---------------------------- AST -------------------------")
    val treeVisitor = new JTreeVisitor(true)
    javaCompiler.compilationUnit.getTree.accept(treeVisitor)
    println("\n\n")

    println("------------------------- Traversal ----------------------")
    val refTagger = new ScopedTraverse
    val taggedTree = refTagger.traverse(tree)
    println(taggedTree)

    println("---------------------------- ENV -------------------------")
    javaCompiler.printEnvs()

    println()
    println("---------------------------- IR  -------------------------")
    val ir = compiler.Compiler.compile(taggedTree)
    println(ir.toString)

    println()
    println("------------------------ Running -------------------------")
    compileAndRun(taggedTree)

  }

  private def compileAndRun(compilationUnit: CompilationUnit): Unit = {
    val compRes = Compiler.compile(compilationUnit)
    val defs = compRes._1
    val mainObjectName = compRes._2

    val writer = new java.io.PrintWriter(System.out)
    try {
      val printer = new IRTreePrinter(writer)
      defs foreach { d =>
        printer.printTopLevelTree(d)
      }
    } finally {
      writer.flush()
    }

    val linked = Linker.link(defs, new ScalaConsoleLogger)

    // Clearly separate the output of the program from the compiling logs
    println("")
    println("")

    Runner.run(mainObjectName, linked, NullLogger, ConsoleJSConsole)
  }

}
