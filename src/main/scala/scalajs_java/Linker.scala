package scalajs_java

import org.scalajs.core.ir
import ir.Printers._
import org.scalajs.core.ir.Infos.ClassInfo
import org.scalajs.core.ir.Trees.ClassDef
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.linker.{Linker => ScalaJSLinker, _}

/**
  * The code is from:
  * https://github.com/sjrd/scalajsir-calc/blob/master/src/main/scala/calc/Linker.scala
  */
object Linker {

  private val libraryIRFiles = {
    // Load the standard library
    val libraryName = "scalajs-library_2.11-0.6.8.jar"
    val libraryJarStream = getClass.getResourceAsStream(libraryName)
    val libraryBytes = try {
      scala.reflect.io.Streamable.bytes(libraryJarStream)
    } finally {
      libraryJarStream.close()
    }

    val libraryVirtualFile = {
      (new MemVirtualBinaryFile(libraryName) with VirtualJarFile)
        .withContent(libraryBytes)
        .withVersion(Some(libraryName)) // unique
    }
    val cache = (new IRFileCache).newCache
    cache.cached(List(IRFileCache.IRContainer.Jar(libraryVirtualFile)))
  }

  def mkIRFile(classDef: ir.Trees.ClassDef): VirtualScalaJSIRFile = {
    new VirtualScalaJSIRFile {
      def path: String = classDef.name.name + ".sjsir"
      def exists: Boolean = true

      val infoAndTree: (ir.Infos.ClassInfo, ir.Trees.ClassDef) =
        (ir.Infos.generateClassInfo(classDef), classDef)
    }
  }

  def link(classDefs: List[ir.Trees.ClassDef], logger: Logger): VirtualJSFile = {
    // Put the `classDef` in a virtual file
    val classIRFiles = classDefs.map(mkIRFile)

    val allIRFiles = libraryIRFiles ++ classIRFiles

    val linker = ScalaJSLinker(
      frontendConfig = frontend.LinkerFrontend.Config().withCheckIR(true))

    val output = WritableMemVirtualJSFile("output.js")
    linker.link(allIRFiles, output, logger)
    output
  }

}