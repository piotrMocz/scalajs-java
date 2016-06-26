package scalajs_java

import org.scalajs.core.ir
import ir.Printers._
import org.scalajs.core.tools.logging._
import org.scalajs.jsenv._

import scalajs_java.compiler.passes._
import scalajs_java.trees._

object Main {

  def main(args: Array[String]): Unit = {
    val javaCompiler = new CompilerInterface()
    javaCompiler.compile(Config.testFilePath)

    println("------------------------- Scala AST ----------------------")
    val tree = (new JTraversePass).run(javaCompiler.compilationUnit)
    println(tree.toString)
    println("\n\n")

    println("------------------------- Traversals ----------------------")

    println("[Operation transforming]")
    val opTree = (new OpTraversePass).run(tree)
    println(opTree)
    println("\n\n")


    val taggedTree = (new RefTagPass).run(opTree)
    println("[reference tagging]")
    println(taggedTree)
    println("\n\n")

    println("---------------------------- IR  -------------------------")
    val ir = (new CompilerPass).run(taggedTree)
    println(ir)
    println("\n\n")


    println("------------------------ Running -------------------------")
    compileAndRun(taggedTree)

  }

  private def compileAndRun(compilationUnit: CompilationUnit): Unit = {
    val compRes = (new CompilerPass).run(compilationUnit)
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
