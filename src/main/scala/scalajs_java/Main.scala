package scalajs_java

import scala.io.StdIn
import scalajs_java.compiler.CompilerPipeline
import scalajs_java.runtime.Config

object Main {

  def main(args: Array[String]): Unit = {
    val argList = args.toList
    val compiler = new CompilerPipeline
    var projPath = ""

    if (argList.length < 1) {
      projPath = new java.io.File(".").getCanonicalPath
      compiler.run(projPath)
    } else {
      val exampleNrOpt =
        if (args.length >= 2) Some(args(1).toInt)
        else None

      if (args(0).equals("example"))
        runExamples(exampleNrOpt)
      else {
        projPath = args(0)
        compiler.run(projPath)
      }
    }
  }

  def runExamples(exampleNrOpt: Option[Int]): Unit = {
    exampleNrOpt match {
      case Some(nr) =>
        runExampleNr(nr)

      case None =>
        val nr = getExampleNr
        runExampleNr(nr)
    }
  }

  def getExampleNr: Int = {
    printExamples()
    print("Please enter the number of an example you'd like to run: ")
    StdIn.readInt()
  }

  def runExampleNr(exNr: Int): Unit = {
    if (exNr < 0 || exNr >= Config.examplesCnt) {
      println("Wrong example number.")
      runExampleNr(getExampleNr)
      return
    }

    val projPath = "examples/" + Config.examples(exNr)
    val compiler = new CompilerPipeline
    println("Running path: " + projPath)
    compiler.run(projPath)
  }

  def printExamples(): Unit = {
    println("Available examples:")

    (0 until Config.examplesCnt).zip(Config.examples).foreach {
      case (nr, exName) => println(s"[$nr] $exName")
    }
  }

}
