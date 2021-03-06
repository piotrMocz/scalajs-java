package scalajs_java.runtime

/**
  * Temporary: here we'll keep all the hard-coded config
  * until we have a fully working compiler.
  */
object Config {
  val testFilePath: String = "Test.java"
  val testProjectPath: String = "examples/assignments"
  val verbose: Boolean = true

  val examples = List(
    "assignments",
    "basic_ops",
    "control_structures",
    "simple_classes",
    "static_members",
    "arrays",
    "interfaces",
    "simple_generics",
    "lambdas"
  )

  val examplesCnt = examples.length
}
