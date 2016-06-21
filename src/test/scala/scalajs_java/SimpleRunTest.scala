package scalajs_java

/*
 * The file is based on:
 * https://github.com/sjrd/scalajsir-calc/blob/master/src/test/scala/calc/RunTest.scala
 * by @sjrd
 */

import org.junit.Test
import org.junit.Assert._
import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import org.scalajs.core.tools.logging._
import org.scalajs.jsenv.JSConsole

import scalajs_java.compiler.Compiler
import scalajs_java.traversals.{JTreeTraverse, OperationsTraverse, ScopedTraverse}

/** Blackbox tests */
class SimpleRunTest {

  private def wrapperMainClass(s: String): String =
    s"""
      |class Test {
      |  public static void main(String[] args) {
      |    $s
      |  }
      |}
    """.stripMargin

  private def assertRun(expected: Any, code: String): Unit = {
    val source = wrapperMainClass(code)

    val javaCompiler = new CompilerInterface()
    javaCompiler.compile("Test", source)

    val tree = JTreeTraverse.traverse(javaCompiler.compilationUnit)
    val opTransformer = new OperationsTraverse
    val opTree = opTransformer.traverse(tree)
    val refTagger = new ScopedTraverse
    val taggedTree = refTagger.traverse(opTree)

    val compRes = Compiler.compile(taggedTree)
    val classDefs = compRes._1
    val mainObjectName = encodeClassName("Test") + "$"

    val linked = Linker.link(classDefs, NullLogger)

    val lines = new java.io.StringWriter
    val console = new JSConsole {
      def log(msg: Any): Unit = lines.append(msg.toString + "\n")
    }

    Runner.run(mainObjectName, linked, NullLogger, console)

    assertEquals(expected.toString + "\n0", lines.toString.trim)
  }

  @Test def runPrintLiteral(): Unit = {
    assertRun(42, "System.out.println(42);")
    assertRun(42L, "System.out.println(42L);")
    assertRun(54.3, "System.out.println(54.3);")
    assertRun("hello", "System.out.println(\"hello\");")
  }

  @Test def runAssignment(): Unit = {
    assertRun(42, "int x = 42; System.out.println(x);")
    assertRun(42, "int x; x = 42; System.out.println(x);")
    assertRun(42, "int x; System.out.println(x = 42);")
  }

  @Test def runIncrement(): Unit = {
    assertRun(43, "int x = 42; x++; System.out.println(x);")
    assertRun(43, "int x = 42; ++x; System.out.println(x);")
    assertRun(42, "int x = 42; System.out.println(x++);")
    assertRun(43, "int x = 42; System.out.println(++x);")
  }

  @Test def runDecrement(): Unit = {
    assertRun(41, "int x = 42; x--; System.out.println(x);")
    assertRun(41, "int x = 42; --x; System.out.println(x);")
    assertRun(42, "int x = 42; System.out.println(x--);")
    assertRun(41, "int x = 42; System.out.println(--x);")
  }

  @Test def runForLoop(): Unit = {
    assertRun("42\n42\n42", "for (int i = 0; i < 3; i++) System.out.println(42);")
    assertRun("0\n1\n2", "for (int i = 0; i < 3; i++) System.out.println(i);")
    assertRun("2\n1\n0", "for (int i = 2; i >= 0; i--) System.out.println(i);")
  }

  @Test def runWhileLoop(): Unit = {
    assertRun("42\n42\n42", "int i = 0; while (i < 3) { System.out.println(42); i++; }")
    assertRun("0\n1\n2", "int i = 0; while (i < 3) { System.out.println(i); i++; }")
    assertRun("2\n1\n0", "int i = 2; while (i >= 0) { System.out.println(i); i--; }")
  }

  @Test def runDoWhileLoop(): Unit = {
    assertRun("42\n42\n42", "int i = 0; do { System.out.println(42); i++; } while (i < 3);")
    assertRun("0\n1\n2", "int i = 0; do { System.out.println(i); i++; } while (i < 3);")
    assertRun("3\n2\n1\n0", "int i = 3; do { System.out.println(i); i--; } while (i >= 0);")
  }
}
