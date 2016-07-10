package scalajs_java

/*
 * The file is based on:
 * https://github.com/sjrd/scalajsir-calc/blob/master/src/test/scala/calc/RunTest.scala
 * by @sjrd
 */

import org.junit.Assert._
import org.junit.Test
import org.scalajs.core.ir.Definitions._
import org.scalajs.core.tools.logging._
import org.scalajs.jsenv.JSConsole

import scalajs_java.compiler.passes._
import scalajs_java.utils.Scope

/** Blackbox tests */
class SimpleRunTest {

  private def wrapperMainClass(defs: String, s: String,
      pkgName: String): String =
    s"""
      |${if (pkgName.isEmpty) "" else "package " + pkgName + ";"}
      |
      |class Test {
      |  $defs
      |
      |  public static void main(String[] args) {
      |    $s
      |  }
      |}
    """.stripMargin

  private def assertRun(expected: Any, code: String, defs: String="",
      pkgName: String=""): Unit = {
    val source = wrapperMainClass(defs, code, pkgName)

    val javaCompiler = new CompilerInterface()
    javaCompiler.compile("Test", source)

    val tree = (new JTraversePass).run(javaCompiler.compilationUnit)
    val opTree = (new OpTraversePass).run(tree)
    val taggedTree = new RefTagPass(scope = Scope.empty).run(opTree)
    val fullTree = (new EnclClassPass).run(taggedTree)
    val sip = new StaticInitsPass
    sip.run(fullTree)

    val compRes = new CompilerPass(sip.inits).run(fullTree)
    val classDefs = compRes._1
    val className = if (pkgName.isEmpty) "Test" else pkgName + ".Test"
    val mainObjectName = encodeClassName(className) + "$"

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

  @Test def runUnqualifiedStaticMethod(): Unit = {
    assertRun("42",
      """
        |foo();
      """.stripMargin,
      """
        |static void foo() {
        |  System.out.println(42);
        |}
      """.stripMargin)

    assertRun("42",
      """
        |System.out.println(foo());
      """.stripMargin,
      """
        |static int foo() {
        |  return 42;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |System.out.println(foo(21));
      """.stripMargin,
      """
        |static int foo(int x) {
        |  return x * 2;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |System.out.println(foo(9, 10, 11, 12));
      """.stripMargin,
      """
        |static int foo(int x, int y, int z, int w) {
        |  return x + y + z + w;
        |}
      """.stripMargin)
  }

  @Test def runStaticMethod(): Unit = {
    assertRun("42",
      """
        |Test.foo();
      """.stripMargin,
      """
        |static void foo() {
        |  System.out.println(42);
        |}
      """.stripMargin)

    assertRun("42",
      """
        |System.out.println(Test.foo());
      """.stripMargin,
      """
        |static int foo() {
        |  return 42;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |System.out.println(Test.foo(21));
      """.stripMargin,
      """
        |static int foo(int x) {
        |  return x * 2;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |System.out.println(Test.foo(9, 10, 11, 12));
      """.stripMargin,
      """
        |static int foo(int x, int y, int z, int w) {
        |  return x + y + z + w;
        |}
      """.stripMargin)
  }

  @Test def runStaticFieldAccess(): Unit = {
    assertRun("42",
      """
        |x = 42;
        |System.out.println(x);
      """.stripMargin,
      """
        |static int x;
      """.stripMargin)

    assertRun("42",
      """
        |Test.x = 42;
        |System.out.println(x);
      """.stripMargin,
      """
        |static int x;
      """.stripMargin)

    assertRun("42",
      """
        |x = 42;
        |System.out.println(Test.x);
      """.stripMargin,
      """
        |static int x;
      """.stripMargin)

    assertRun("42",
      """
        |Test.x = 42;
        |System.out.println(Test.x);
      """.stripMargin,
      """
        |static int x;
      """.stripMargin)
  }

  @Test def runPackageName(): Unit = {
    assertRun("42",
      """
        |test.Test.x = 42;
        |System.out.println(test.Test.x);
      """.stripMargin,
      """
        |static int x;
      """.stripMargin,
      pkgName="test")

    assertRun("42",
      """
        |test.Test.foo();
      """.stripMargin,
      """
        |static void foo() {
        |  System.out.println(42);
        |}
      """.stripMargin,
      pkgName="test")

    assertRun("42",
      """
        |Test t = new Test();
        |t.foo();
      """.stripMargin,
      """
        |void foo() { System.out.println(42); }
      """.stripMargin,
      pkgName="test")

    assertRun("42",
      """
        |Test t = new test.Test();
        |t.foo();
      """.stripMargin,
      """
        |void foo() { System.out.println(42); }
      """.stripMargin,
      pkgName="test")

    assertRun("42",
      """
        |test.Test t = new Test();
        |t.foo();
      """.stripMargin,
      """
        |void foo() { System.out.println(42); }
      """.stripMargin,
      pkgName="test")

    assertRun("42",
      """
        |test.Test t = new test.Test();
        |t.foo();
      """.stripMargin,
      """
        |void foo() { System.out.println(42); }
      """.stripMargin,
      pkgName="test")
  }

  @Test def runShadowing(): Unit = {
    assertRun("42",
      """
        |Test.x = 24;
        |System.out.println(foo(42));
      """.stripMargin,
      """
        |static int x;
        |
        |static int foo(int x) {
        |  return x;
        |}
      """.stripMargin)

    assertRun("24",
      """
        |Test.x = 24;
        |System.out.println(foo(42));
      """.stripMargin,
      """
        |static int x;
        |
        |static int foo(int x) {
        |  return Test.x;
        |}
      """.stripMargin)

    assertRun("66",
      """
        |Test.x = 24;
        |System.out.println(foo(42));
      """.stripMargin,
      """
        |static int x;
        |
        |static int foo(int x) {
        |  return Test.x + x;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |Test test = new Test();
        |test.x = 24;
        |System.out.println(test.foo(42));
      """.stripMargin,
      """
        |int x;
        |
        |int foo(int x) {
        |  return x;
        |}
      """.stripMargin)

    assertRun("24",
      """
        |Test test = new Test();
        |test.x = 24;
        |System.out.println(test.foo(42));
      """.stripMargin,
      """
        |int x;
        |
        |int foo(int x) {
        |  return this.x;
        |}
      """.stripMargin)

    assertRun("66",
      """
        |Test test = new Test();
        |test.x = 24;
        |System.out.println(test.foo(42));
      """.stripMargin,
      """
        |int x;
        |
        |int foo(int x) {
        |  return this.x + x;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |Test.x = 24;
        |System.out.println(foo());
      """.stripMargin,
      """
        |static int x;
        |
        |static int foo() {
        |  int x = 42;
        |  return x;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |Test test = new Test();
        |test.x = 24;
        |System.out.println(test.foo());
      """.stripMargin,
      """
        |int x;
        |
        |int foo() {
        |  int x = 42;
        |  return x;
        |}
      """.stripMargin)
  }

  @Test def runStaticFieldInit(): Unit = {
    assertRun("42",
      """
        |System.out.println(x);
      """.stripMargin,
      """
        |static int x = 42;
      """.stripMargin)

    assertRun("42",
      """
        |System.out.println(Test.x);
      """.stripMargin,
      """
        |static int x = 42;
      """.stripMargin)
  }

  @Test def runObjectTypeMethods(): Unit = {
    assertRun("42",
      """
        |Test test = getTest();
        |System.out.println(test.foo());
      """.stripMargin,
      """
        |static Test getTest() {
        |  return new Test();
        |}
        |
        |int foo() {
        |  return 42;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |Test test = new Test();
        |Test test2 = testId(test);
        |System.out.println(test2.foo());
      """.stripMargin,
      """
        |static Test testId(Test t) {
        |  return t;
        |}
        |
        |int foo() {
        |  return 42;
        |}
      """.stripMargin)

    assertRun("42",
      """
        |Test test = new Test(21);
        |Test test2 = modifyTest(test);
        |System.out.println(test2.x);
      """.stripMargin,
      """
        |static Test modifyTest(Test t) {
        |  t.x *= 2;
        |  return t;
        |}
        |
        |int x;
        |
        |Test(int x) {
        |  this.x = x;
        |}
      """.stripMargin)
  }
}
