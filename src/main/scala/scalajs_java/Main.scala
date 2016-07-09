package scalajs_java

import scalajs_java.compiler.CompilerPipeline

object Main {

  def main(args: Array[String]): Unit = {
    val compiler = new CompilerPipeline
    compiler.run(Config.testProjectPath)
  }

}
