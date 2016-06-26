package scalajs_java.compiler.passes

import scalajs_java.utils.ErrorHanlder

trait Pass[A, B] {

  val verbose: Boolean

  val name: String

  val errorHandler: ErrorHanlder

  def runPass(tree: A): B

  def printHeader(): Unit =
    println("-" * 10 + name + "-" * 10)

  def run(tree: A): B = {
    if (verbose) printHeader()

    val result = this.runPass(tree)
    this.errorHandler.reportErrors()

    if (verbose) println(result.toString + "\n\n")
    result
  }
}
