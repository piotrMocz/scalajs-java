package scalajs_java.compiler.passes

import scalajs_java.utils.ErrorHandler

trait Pass[A, B] {

  val verbose: Boolean

  val name: String

  val errorHandler: ErrorHandler

  def runPass(tree: A): B

  def printHeader(): Unit =
    println("-" * 15 + " " + name + " " + "-" * 15)

  def run(tree: A): B = {
    if (verbose) printHeader()

    val result = this.runPass(tree)
    this.errorHandler.reportErrors()

    if (verbose) println(result.toString + "\n\n")
    result
  }
}
