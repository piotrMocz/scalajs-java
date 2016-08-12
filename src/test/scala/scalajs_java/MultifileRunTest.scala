package scalajs_java

/*
 * The file is based on:
 * https://github.com/sjrd/scalajsir-calc/blob/master/src/test/scala/calc/RunTest.scala
 * by @sjrd
 */

import org.junit.Assert._
import org.junit.Test
import org.scalajs.core.tools.logging._
import org.scalajs.jsenv.JSConsole

import scala.collection.JavaConversions._
import scalajs_java.compiler.CompilerPipeline

/** Blackbox tests for multiple files */
class MultifileRunTest {

  private def wrapperMainClass(s: String, pkgName: String): String =
    s"""
       |${if (pkgName.isEmpty) "" else "package " + pkgName + ";"}
       |
       |class Test {
       |
       |  public static void main(String[] args) {
       |    $s
       |  }
       |}
    """.stripMargin

  private def wrapperClass(clsName: String, defs: String,
                           pkgName: String): String =
    s"""
       |${if (pkgName.isEmpty) "" else "package " + pkgName + ";"}
       |
       |class $clsName {
       |  $defs
       |}
    """.stripMargin

  private def assertRun(expected: Any, mainCode: String,
                        classes: List[(String, String)],
                        pkgName: String=""): Unit = {
    val mainClassSource = wrapperMainClass(mainCode, pkgName)
    val sources = classes.map(cls => wrapperClass(cls._1, cls._2, pkgName))
    val allSources = mainClassSource :: sources
    val allClassNames = "Test" :: classes.map(_._1.takeWhile(_ != '<'))

    val javaCompiler = new CompilerInterface()
    javaCompiler.compileVirtualProject(allClassNames, allSources)
    val compilerPipeline = new CompilerPipeline(verbose = false)
    val compResults = compilerPipeline.runPasses(javaCompiler.compilationUnits.toList)

    val classDefs = compResults._1
    val mainObjectName = compResults._2

    val linked = Linker.link(classDefs, NullLogger)

    val lines = new java.io.StringWriter
    val console = new JSConsole {
      def log(msg: Any): Unit = lines.append(msg.toString + "\n")
    }

    Runner.run(mainObjectName, linked, NullLogger, console)

    assertEquals(expected.toString + "\n0", lines.toString.trim)
  }

  @Test def runFieldAccess(): Unit = {
    assertRun("0",
      """
        |Test2 t = new Test2();
        |System.out.println(t.x);
      """.stripMargin,
      List(("Test2",
          """
            |int x;
          """.stripMargin)))

    assertRun("42",
    """
      |Test2 t = new Test2();
      |System.out.println(t.x);
    """.stripMargin,
      List(("Test2",
          """
            |int x;
            |
            |Test2() {
            |  this.x = 42;
            |}
          """.stripMargin)))
  }

  @Test def runStaticFieldAccess(): Unit = {
    assertRun("0",
      """
        |System.out.println(Test2.x);
      """.stripMargin,
      List(("Test2",
          """
            |static int x;
          """.stripMargin)))

    assertRun("42",
      """
        |Test2.x = 42;
        |System.out.println(Test2.x);
      """.stripMargin,
      List(("Test2",
          """
            |static int x;
          """.stripMargin)))

    assertRun("42",
      """
        |System.out.println(Test2.test3.x);
      """.stripMargin,
      List(("Test2",
          """
            |static Test3 test3 = new Test3(42);
          """.stripMargin),
        ("Test3",
          """
            |int x;
            |
            |Test3(int x) {
            |  this.x = x;
            |}
          """.stripMargin)))
  }

  @Test def runMethodCall(): Unit = {
    assertRun("42",
      """
        |Test2 t = new Test2();
        |t.foo();
      """.stripMargin,
      List(("Test2",
          """
            |void foo() {
            |  System.out.println(42);
            |}
          """.stripMargin)))

    assertRun("42",
      """
        |Test2 t = new Test2();
        |System.out.println(t.foo());
      """.stripMargin,
      List(("Test2",
          """
            |int foo() {
            |  return 42;
            |}
          """.stripMargin)))

    assertRun("42",
      """
        |Test2 t = new Test2(42);
        |System.out.println(t.foo());
      """.stripMargin,
      List(("Test2",
          """
            |int x;
            |
            |Test2(int p) {
            |  this.x = p;
            |}
            |
            |int foo() {
            |  return this.x;
            |}
          """.stripMargin)))
  }

  @Test def runMethodCallPkg(): Unit = {
    assertRun("42",
      """
        |Test2 t = new Test2();
        |t.foo();
      """.stripMargin,
      List(("Test2",
          """
            |void foo() {
            |  System.out.println(42);
            |}
          """.stripMargin)),
      "test")

    assertRun("42",
      """
        |Test2 t = new Test2();
        |System.out.println(t.foo());
      """.stripMargin,
      List(("Test2",
          """
            |int foo() {
            |  return 42;
            |}
          """.stripMargin)),
      "test")

    assertRun("42",
      """
        |Test2 t = new Test2(42);
        |System.out.println(t.foo());
      """.stripMargin,
      List(("Test2",
          """
            |int x;
            |
            |Test2(int p) {
            |  this.x = p;
            |}
            |
            |int foo() {
            |  return this.x;
            |}
          """.stripMargin)),
      "test")
  }

  @Test def runStaticMethodCall(): Unit = {
    assertRun("42",
      """
        |Test2.foo();
      """.stripMargin,
      List(("Test2",
          """
            |static void foo() {
            |  System.out.println(42);
            |}
          """.stripMargin)))

    assertRun("42",
      """
        |System.out.println(Test2.foo(42));
      """.stripMargin,
      List(("Test2",
          """
            |static int foo(int x) {
            |  return x;
            |}
          """.stripMargin)))

    assertRun("42",
      """
        |Test2.x = 42;
        |Test2.foo();
      """.stripMargin,
      List(("Test2",
          """
            |static int x;
            |
            |static void foo() {
            |  System.out.println(Test2.x);
            |}
          """.stripMargin)))
  }

  @Test def runShadowing(): Unit = {
    assertRun("42",
      """
        |Test2.x = 13;
        |Test3.x = 24;
        |int x = 42;
        |System.out.println(x);
      """.stripMargin,
      List(
        ("Test2",
          """
            |static int x;
          """.stripMargin),
        ("Test3",
          """
            |static int x;
          """.stripMargin)))

    assertRun("79",
      """
        |Test2.x = 13;
        |Test3.x = 24;
        |int x = 42;
        |System.out.println(x + Test2.x + Test3.x);
      """.stripMargin,
      List(
        ("Test2",
          """
            |static int x;
          """.stripMargin),
        ("Test3",
          """
            |static int x;
          """.stripMargin)))
  }

  @Test def runGenericFields(): Unit = {
    assertRun("42",
      """
        |Test2<Integer> test2 = new Test2<Integer>(42);
        |System.out.println(test2.y);
      """.stripMargin,
      List(
        ("Test2<T>",
          """
            |T y;
            |
            |Test2(T y) {
            |  this.y = y;
            |}
          """.stripMargin)))

    assertRun("42",
      """
        |Test2<Integer> test2 = new Test2<Integer>(0);
        |test2.y = 42;
        |System.out.println(test2.y);
      """.stripMargin,
      List(
        ("Test2<T>",
            """
              |T y;
              |
              |Test2(T y) {
              |  this.y = y;
              |}
            """.stripMargin)))

    assertRun("42",
      """
        |Test3 test3 = new Test3(42);
        |Test2<Test3> test2 = new Test2<Test3>(test3);
        |System.out.println(test2.y.x);
      """.stripMargin,
      List(
        ("Test2<T>",
          """
            |T y;
            |
            |Test2(T y) {
            |  this.y = y;
            |}
          """.stripMargin),
        ("Test3",
          """
            |int x;
            |
            |Test3(int x) {
            |  this.x = x;
            |}
          """.stripMargin)))
  }

  @Test def runGenericFields2(): Unit = {
    assertRun("42",
      """
        |Test2<Integer> test2 = new Test2<Integer>(42);
        |int x = test2.y;
        |System.out.println(x);
      """.stripMargin,
      List(
        ("Test2<T>",
            """
              |T y;
              |
              |Test2(T y) {
              |  this.y = y;
              |}
            """.stripMargin)))

    assertRun("42",
      """
        |Test3 test3 = new Test3(42);
        |Test2<Test3> test2 = new Test2<Test3>(test3);
        |int x = test2.y.x;
        |System.out.println(x);
      """.stripMargin,
      List(
        ("Test2<T>",
            """
              |T y;
              |
              |Test2(T y) {
              |  this.y = y;
              |}
            """.stripMargin),
        ("Test3",
            """
              |int x;
              |
              |Test3(int x) {
              |  this.x = x;
              |}
            """.stripMargin)))

    assertRun("42",
      """
        |Test3 test3 = new Test3(42);
        |Test2<Test3> test2 = new Test2<Test3>(test3);
        |Test3 t3 = test2.y;
        |System.out.println(t3.x);
      """.stripMargin,
      List(
        ("Test2<T>",
            """
              |T y;
              |
              |Test2(T y) {
              |  this.y = y;
              |}
            """.stripMargin),
        ("Test3",
            """
              |int x;
              |
              |Test3(int x) {
              |  this.x = x;
              |}
            """.stripMargin)))
  }

  @Test def runGenericBinops(): Unit = {
    assertRun("55",
      """
        |Test2<Integer> test2 = new Test2<Integer>(42);
        |System.out.println(test2.y + 13);
      """.stripMargin,
      List(
        ("Test2<T>",
          """
            |T y;
            |
            |Test2(T y) {
            |  this.y = y;
            |}
          """.stripMargin)))

    assertRun("84",
      """
        |Test2<Integer> test2 = new Test2<Integer>(42);
        |System.out.println(test2.y + test2.y);
      """.stripMargin,
      List(
        ("Test2<T>",
          """
            |T y;
            |
            |Test2(T y) {
            |  this.y = y;
            |}
          """.stripMargin)))

  }

  @Test def runObjectArrays(): Unit = {
    assertRun("42",
      """
        |Test2 test2 = new Test2(42);
        |Test2[] testArr = new Test2[1];
        |testArr[0] = test2;
        |System.out.println(testArr[0].x);
      """.stripMargin,
      List(
      ("Test2",
        """
          |int x;
          |
          |Test2(int x) {
          |  this.x = x;
          |}
        """.stripMargin)))

    assertRun("42",
      """
        |Test2 test2 = new Test2(42);
        |Test2[] testArr = { test2 };
        |System.out.println(testArr[0].x);
      """.stripMargin,
      List(
        ("Test2",
          """
            |int x;
            |
            |Test2(int x) {
            |  this.x = x;
            |}
          """.stripMargin)))

    assertRun("null",
      """
        |Test2 test2 = new Test2(42);
        |Test2[] testArr = { null, test2 };
        |System.out.println(testArr[0]);
      """.stripMargin,
      List(
        ("Test2",
          """
            |int x;
            |
            |Test2(int x) {
            |  this.x = x;
            |}
          """.stripMargin)))
  }

//  @Test def runInheritance(): Unit = {
//    assertRun("42",
//      """
//        |Test3 test3 = new Test3(42);
//        |Test2<Test3> test2 = new Test2<Test3>(test3);
//        |Test3 t3 = test2.y;
//        |System.out.println(t3.x);
//      """.stripMargin,
//      List(
//        ("Test2<T>",
//          """
//            |T y;
//            |
//            |Test2(T y) {
//            |  this.y = y;
//            |}
//          """.stripMargin),
//        ("Test3",
//          """
//            |int x;
//            |
//            |Test3(int x) {
//            |  this.x = x;
//            |}
//          """.stripMargin)))
//  }
}
