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
import scalajs_java.compiler.passes.{CompilerPass, JTraversePass, OpTraversePass, RefTagPass}
import scalajs_java.traversals.{JTreeTraverse, OperationsTraverse, ScopedTraverse}

/** Blackbox tests */
class SimpleRunTest {

  private def wrapperMainClass(defs: String, s: String): String =
    s"""
      |class Test {
      |  $defs
      |
      |  public static void main(String[] args) {
      |    $s
      |  }
      |}
    """.stripMargin

  private def assertRun(expected: Any, code: String, defs: String=""): Unit = {
    val source = wrapperMainClass(defs, code)

    val javaCompiler = new CompilerInterface()
    javaCompiler.compile("Test", source)

    val tree = (new JTraversePass).run(javaCompiler.compilationUnit)
    val opTree = (new OpTraversePass).run(tree)
    val taggedTree = (new RefTagPass).run(opTree)

    val compRes = (new CompilerPass).run(taggedTree)
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

  @Test def runArrayAccess(): Unit = {
    assertRun("42", "int[] arr = {41, 42, 43}; System.out.println(arr[1]);")
    assertRun("0", "int[] arr = new int[5]; System.out.println(arr[2]);")

    assertRun("0\n1\n2\n3\n4",
      """
      |int[] arr = {0, 1, 2, 3, 4};
      |for (int i = 0; i < 5; i++)
      |  System.out.println(arr[i]);
      """.stripMargin)

    assertRun("0\n0\n0\n0\n0",
      """
      |int[] arr = new int[5];
      |for (int i = 0; i < 5; i++)
      |  System.out.println(arr[i]);
      """.stripMargin)
  }

  @Test def runTwoDimArrays(): Unit = {
    assertRun("0", "int[][] arr = new int[3][5]; System.out.println(arr[1][1]);")

    assertRun("42",
      """
        |int[][] arr = new int[3][5];
        |arr[1][2] = 42;
        |System.out.println(arr[1][2]);
      """.stripMargin)

    assertRun("0\n0\n0\n0\n0\n0\n0\n0\n0",
      """
        |int[][] arr = new int[3][3];
        |for (int i = 0; i < 3; ++i)
        |  for (int j = 0; j < 3; ++j)
        |    System.out.println(arr[i][j]);
      """.stripMargin)
  }

  @Test def runFieldAccess(): Unit = {
    assertRun("42",
    """
      |Test t = new Test();
      |System.out.println(t.x);
    """.stripMargin,
    """
      |int x;
      |
      |Test() {
      |  this.x = 42;
      |}
    """.stripMargin)

    assertRun("42",
      """
        |Test t = new Test(42);
        |System.out.println(t.x);
      """.stripMargin,
      """
        |int x;
        |
        |Test(int par) {
        |  this.x = par;
        |}
      """.stripMargin)

    assertRun("66",
      """
        |Test t = new Test(42, 24);
        |System.out.println(t.x + t.y);
      """.stripMargin,
      """
        |int x;
        |int y;
        |
        |Test(int par1, int par2) {
        |  this.x = par1;
        |  this.y = par2;
        |}
      """.stripMargin)
  }

  @Test def runMethodCall(): Unit = {
    assertRun("42",
      """
        |Test t = new Test();
        |t.foo();
      """.stripMargin,
      """
        |void foo() { System.out.println(42); }
      """.stripMargin)

    assertRun("42",
      """
        |Test t = new Test();
        |System.out.println(t.foo());
      """.stripMargin,
      """
        |int foo() { return 42; }
      """.stripMargin)

    assertRun("42",
      """
        |Test t = new Test(42);
        |System.out.println(t.foo());
      """.stripMargin,
      """
        |int x;
        |
        |Test(int par) {
        |  this.x = par;
        |}
        |
        |int foo() { return this.x; }
      """.stripMargin)

    assertRun("66",
      """
        |Test t = new Test(42);
        |System.out.println(t.foo(24));
      """.stripMargin,
      """
        |int x;
        |
        |Test(int par) {
        |  this.x = par;
        |}
        |
        |int foo(int mpar) { return this.x + mpar; }
      """.stripMargin)

    assertRun("76",
      """
        |Test t = new Test(42);
        |System.out.println(t.foo(24, 10));
      """.stripMargin,
      """
        |int x;
        |
        |Test(int par) {
        |  this.x = par;
        |}
        |
        |int foo(int mpar1, int mpar2) { return this.x + mpar1 + mpar2; }
      """.stripMargin)

  }
}
