
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency

import org.scalajs.jsenv._

/**
  * The code is from:
  * https://github.com/sjrd/scalajsir-calc/blob/master/src/main/scala/calc/Linker.scala
  */

object Runner {

  def run(jsFile: VirtualJSFile, logger: Logger, console: JSConsole): Unit = {
    import org.scalajs.jsenv._

    val jsEnv = new nodejs.NodeJSEnv()
      .loadLibs(Seq(ResolvedJSDependency.minimal(jsFile)))

    val code =
      s"""console.log(${Compiler.MainObjectFullName}().main());\n"""
    val codeFile = new MemVirtualJSFile("maincode.js")
      .withContent(code)
      .withVersion(Some("maincode.js")) // unique

    val runner = jsEnv.jsRunner(codeFile)

    runner.run(logger, console)
  }

}