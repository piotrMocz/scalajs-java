package scalajs_java.compiler.passes

import scalajs_java.utils.ErrorHanlder

trait Pass[A, B] {

  private val verbose: Boolean = false

  val errorHandler: ErrorHanlder

  def runPass(tree: A): B

  def run(tree: A): B = {
    val result = this.runPass(tree)
    this.errorHandler.reportErrors()
    result
  }
}
