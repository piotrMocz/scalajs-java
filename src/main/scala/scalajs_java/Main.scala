package scalajs_java

import org.scalajs.core.ir
import ir.Printers._
import org.scalajs.core.tools.logging._
import org.scalajs.jsenv._

import scalajs_java.compiler.CompilerPipeline
import scalajs_java.compiler.passes._
import scalajs_java.trees._

object Main {

  def main(args: Array[String]): Unit = {
    val compiler = new CompilerPipeline
    compiler.run("testproject")

//    val javaCompiler = new CompilerInterface()
//    javaCompiler.compileProject("testproject")
//    println(javaCompiler.compilationUnits)
//
//    val javaCompiler = new CompilerInterface()
//    javaCompiler.compile(Config.testFilePath)
//
//    val tree = new JTraversePass(verbose = Config.verbose).run(
//        javaCompiler.compilationUnit)
//
//    val opTree = new OpTraversePass(verbose = Config.verbose).run(tree)
//
//    val taggedTree = new RefTagPass(verbose = Config.verbose).run(opTree)
//
//    val fullTree = new EnclClassPass(verbose = Config.verbose).run(taggedTree)
//
//    val ir = new CompilerPass(verbose = Config.verbose).run(fullTree)
//
//    println("------------------------ Running -------------------------")
//    compileAndRun(fullTree)

  }

//  private def compileAndRun(compilationUnit: CompilationUnit): Unit = {
//    val compRes = (new CompilerPass).run(compilationUnit)
//    val defs = compRes._1
//    val mainObjectName = compRes._2.get
//
//    val writer = new java.io.PrintWriter(System.out)
//    try {
//      val printer = new IRTreePrinter(writer)
//      defs foreach { d =>
//        printer.printTopLevelTree(d)
//      }
//    } finally {
//      writer.flush()
//    }
//
//    val linked = Linker.link(defs, new ScalaConsoleLogger)
//
//    // Clearly separate the output of the program from the compiling logs
//    println("")
//    println("")
//
//    Runner.run(mainObjectName, linked, NullLogger, ConsoleJSConsole)
//  }

}
