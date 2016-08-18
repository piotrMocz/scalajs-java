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

  private def wrapperClass(clsName: String, extendsCl: Option[String],
                           implementsList: List[String], defs: String,
                           pkgName: String, interface: Boolean): String = {
    val extCl = extendsCl match {
      case Some(e) => s"extends $e"
      case None    => ""
    }

    val implList =
      if (implementsList.nonEmpty) s"implements " + implementsList.mkString(", ")
      else ""

    val clsType = if (interface) "interface" else "class"

    s"""
       |${if (pkgName.isEmpty) "" else "package " + pkgName + ";"}
       |
       |$clsType $clsName $extCl $implList {
       |  $defs
       |}
    """.stripMargin
  }

  private def assertRun(expected: Any, mainCode: String,
                        classes: List[(String, Option[String], List[String], String, Boolean)],
                        pkgName: String=""): Unit = {
    val mainClassSource = wrapperMainClass(mainCode, pkgName)
    val sources = classes.map(cls => wrapperClass(cls._1, cls._2, cls._3, cls._4, pkgName, cls._5))
    val allSources = mainClassSource :: sources
    val allClassNames = "Test" :: classes.map(_._1.takeWhile(_ != '<'))

    val javaCompiler = new CompilerInterface()
    javaCompiler.compileVirtualProject(allClassNames, allSources)
    if (javaCompiler.errCount > 0) {
      println(javaCompiler.formatErrors())
      fail()
    }

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
      List(("Test2", None, Nil,
          """
            |int x;
          """.stripMargin, false)))

    assertRun("42",
    """
      |Test2 t = new Test2();
      |System.out.println(t.x);
    """.stripMargin,
      List(("Test2", None, Nil,
          """
            |int x;
            |
            |Test2() {
            |  this.x = 42;
            |}
          """.stripMargin, false)))
  }

  @Test def runStaticFieldAccess(): Unit = {
    assertRun("0",
      """
        |System.out.println(Test2.x);
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |static int x;
          """.stripMargin, false)))

    assertRun("42",
      """
        |Test2.x = 42;
        |System.out.println(Test2.x);
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |static int x;
          """.stripMargin, false)))

    assertRun("42",
      """
        |System.out.println(Test2.test3.x);
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |static Test3 test3 = new Test3(42);
          """.stripMargin, false),
        ("Test3", None, Nil,
          """
            |int x;
            |
            |Test3(int x) {
            |  this.x = x;
            |}
          """.stripMargin, false)))
  }

  @Test def runMethodCall(): Unit = {
    assertRun("42",
      """
        |Test2 t = new Test2();
        |t.foo();
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |void foo() {
            |  System.out.println(42);
            |}
          """.stripMargin, false)))

    assertRun("42",
      """
        |Test2 t = new Test2();
        |System.out.println(t.foo());
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |int foo() {
            |  return 42;
            |}
          """.stripMargin, false)))

    assertRun("42",
      """
        |Test2 t = new Test2(42);
        |System.out.println(t.foo());
      """.stripMargin,
      List(("Test2", None, Nil,
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
          """.stripMargin, false)))
  }

  @Test def runMethodCallPkg(): Unit = {
    assertRun("42",
      """
        |Test2 t = new Test2();
        |t.foo();
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |void foo() {
            |  System.out.println(42);
            |}
          """.stripMargin, false)),
      "test")

    assertRun("42",
      """
        |Test2 t = new Test2();
        |System.out.println(t.foo());
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |int foo() {
            |  return 42;
            |}
          """.stripMargin, false)),
      "test")

    assertRun("42",
      """
        |Test2 t = new Test2(42);
        |System.out.println(t.foo());
      """.stripMargin,
      List(("Test2", None, Nil,
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
          """.stripMargin, false)),
      "test")
  }

  @Test def runStaticMethodCall(): Unit = {
    assertRun("42",
      """
        |Test2.foo();
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |static void foo() {
            |  System.out.println(42);
            |}
          """.stripMargin, false)))

    assertRun("42",
      """
        |System.out.println(Test2.foo(42));
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |static int foo(int x) {
            |  return x;
            |}
          """.stripMargin, false)))

    assertRun("42",
      """
        |Test2.x = 42;
        |Test2.foo();
      """.stripMargin,
      List(("Test2", None, Nil,
          """
            |static int x;
            |
            |static void foo() {
            |  System.out.println(Test2.x);
            |}
          """.stripMargin, false)))
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
        ("Test2", None, Nil,
          """
            |static int x;
          """.stripMargin, false),
        ("Test3", None, Nil,
          """
            |static int x;
          """.stripMargin, false)))

    assertRun("79",
      """
        |Test2.x = 13;
        |Test3.x = 24;
        |int x = 42;
        |System.out.println(x + Test2.x + Test3.x);
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |static int x;
          """.stripMargin, false),
        ("Test3", None, Nil,
          """
            |static int x;
          """.stripMargin, false)))
  }

  @Test def runGenericFields(): Unit = {
    assertRun("42",
      """
        |Test2<Integer> test2 = new Test2<Integer>(42);
        |System.out.println(test2.y);
      """.stripMargin,
      List(
        ("Test2<T>", None, Nil,
          """
            |T y;
            |
            |Test2(T y) {
            |  this.y = y;
            |}
          """.stripMargin, false)))

    assertRun("42",
      """
        |Test2<Integer> test2 = new Test2<Integer>(0);
        |test2.y = 42;
        |System.out.println(test2.y);
      """.stripMargin,
      List(
        ("Test2<T>", None, Nil,
            """
              |T y;
              |
              |Test2(T y) {
              |  this.y = y;
              |}
            """.stripMargin, false)))

    assertRun("42",
      """
        |Test3 test3 = new Test3(42);
        |Test2<Test3> test2 = new Test2<Test3>(test3);
        |System.out.println(test2.y.x);
      """.stripMargin,
      List(
        ("Test2<T>", None, Nil,
          """
            |T y;
            |
            |Test2(T y) {
            |  this.y = y;
            |}
          """.stripMargin, false),
        ("Test3", None, Nil,
          """
            |int x;
            |
            |Test3(int x) {
            |  this.x = x;
            |}
          """.stripMargin, false)))
  }

  @Test def runGenericFields2(): Unit = {
    assertRun("42",
      """
        |Test2<Integer> test2 = new Test2<Integer>(42);
        |int x = test2.y;
        |System.out.println(x);
      """.stripMargin,
      List(
        ("Test2<T>", None, Nil,
            """
              |T y;
              |
              |Test2(T y) {
              |  this.y = y;
              |}
            """.stripMargin, false)))

    assertRun("42",
      """
        |Test3 test3 = new Test3(42);
        |Test2<Test3> test2 = new Test2<Test3>(test3);
        |int x = test2.y.x;
        |System.out.println(x);
      """.stripMargin,
      List(
        ("Test2<T>", None, Nil,
            """
              |T y;
              |
              |Test2(T y) {
              |  this.y = y;
              |}
            """.stripMargin, false),
        ("Test3", None, Nil,
            """
              |int x;
              |
              |Test3(int x) {
              |  this.x = x;
              |}
            """.stripMargin, false)))

    assertRun("42",
      """
        |Test3 test3 = new Test3(42);
        |Test2<Test3> test2 = new Test2<Test3>(test3);
        |Test3 t3 = test2.y;
        |System.out.println(t3.x);
      """.stripMargin,
      List(
        ("Test2<T>", None, Nil,
            """
              |T y;
              |
              |Test2(T y) {
              |  this.y = y;
              |}
            """.stripMargin, false),
        ("Test3", None, Nil,
            """
              |int x;
              |
              |Test3(int x) {
              |  this.x = x;
              |}
            """.stripMargin, false)))
  }

  @Test def runGenericBinops(): Unit = {
    assertRun("55",
      """
        |Test2<Integer> test2 = new Test2<Integer>(42);
        |System.out.println(test2.y + 13);
      """.stripMargin,
      List(
        ("Test2<T>", None, Nil,
          """
            |T y;
            |
            |Test2(T y) {
            |  this.y = y;
            |}
          """.stripMargin, false)))

    assertRun("84",
      """
        |Test2<Integer> test2 = new Test2<Integer>(42);
        |System.out.println(test2.y + test2.y);
      """.stripMargin,
      List(
        ("Test2<T>", None, Nil,
          """
            |T y;
            |
            |Test2(T y) {
            |  this.y = y;
            |}
          """.stripMargin, false)))

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
      ("Test2", None, Nil,
        """
          |int x;
          |
          |Test2(int x) {
          |  this.x = x;
          |}
        """.stripMargin, false)))

    assertRun("42",
      """
        |Test2 test2 = new Test2(42);
        |Test2[] testArr = { test2 };
        |System.out.println(testArr[0].x);
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |int x;
            |
            |Test2(int x) {
            |  this.x = x;
            |}
          """.stripMargin, false)))

    assertRun("null",
      """
        |Test2 test2 = new Test2(42);
        |Test2[] testArr = { null, test2 };
        |System.out.println(testArr[0]);
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |int x;
            |
            |Test2(int x) {
            |  this.x = x;
            |}
          """.stripMargin, false)))
  }

  @Test def runInheritance(): Unit = {
    assertRun("42\n13",
      """
        |Test3 test3 = new Test3(42);
        |System.out.println(test3.x);
        |System.out.println(test3.y);
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |int y;
            |
            |Test2(int y) {
            |  this.y = y;
            |}
          """.stripMargin, false),
        ("Test3", Some("Test2"), Nil,
          """
            |int x;
            |
            |Test3(int x) {
            |  super(13);
            |  this.x = x;
            |}
          """.stripMargin, false)))

    assertRun("42\n13",
      """
        |Test3 test3 = new Test3();
        |System.out.println(test3.test3());
        |System.out.println(test3.test2());
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |int test2() { return 13; }
          """.stripMargin, false),
        ("Test3", Some("Test2"), Nil,
          """
            |int test3() { return 42; }
          """.stripMargin, false)))
  }

  @Test def runPolymorphism(): Unit = {
    assertRun("child",
      """
        |Test2 test3 = new Test3();
        |System.out.println(test3.test());
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |String test() { return "parent"; }
          """.stripMargin, false),
        ("Test3", Some("Test2"), Nil,
          """
            |String test() { return "child"; }
          """.stripMargin, false)))
  }

  @Test def runInterfaces(): Unit = {
    assertRun("42",
      """
        |Test3 test3 = new Test3();
        |System.out.println(test3.test());
      """.stripMargin,
      List(
        ("Test2", None, Nil,
            """
              |public int test();
            """.stripMargin, true),
        ("Test3", None, List("Test2"),
            """
              |public int test() { return 42; }
            """.stripMargin, false)))

    assertRun("42",
      """
        |Test2 test3 = new Test3();
        |System.out.println(test3.test());
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |public int test();
          """.stripMargin, true),
        ("Test3", None, List("Test2"),
          """
            |public int test() { return 42; }
          """.stripMargin, false)))
  }

  @Test def runAnonymousClasses(): Unit = {
    assertRun("42",
      """
        |Test2 test = new Test2() {
        |  public int test() { return 42; }
        |};
        |System.out.println(test.test());
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |public int test();
          """.stripMargin, true)))

    assertRun("42",
      """
        |Test2 test = new Test2() {
        |  public int test(int x) { return x; }
        |};
        |System.out.println(test.test(42));
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |public int test(int x);
          """.stripMargin, true)))
  }

  @Test def runLambdas(): Unit = {
    assertRun("42",
      """
        |Test2 test = () -> { return 42; };
        |System.out.println(test.test());
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |public int test();
          """.stripMargin, true)))

    assertRun("42",
      """
        |Test2 test = (x) -> { return x; };
        |System.out.println(test.test(42));
      """.stripMargin,
      List(
        ("Test2", None, Nil,
          """
            |public int test(int x);
          """.stripMargin, true)))
  }

}
