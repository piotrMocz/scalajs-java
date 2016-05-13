
object Main extends App {

  val compiler = new CompilerInterface()
  compiler.compile("Test2.java")

  println(compiler.compilationUnit.getImports)
  println(compiler.compilationUnit.getPackageName)

  val tree = TreeTraverse.traverse(compiler.compilationUnit)

  println("---------------------------- AST -------------------------")
  val treeVisitor = new JTreeVisitor(false)
  compiler.compilationUnit.getTree.accept(treeVisitor)
  println("----------------------------------------------------------")

  println("---------------------------- ENV -------------------------")
  compiler.printEnvs()
  println("----------------------------------------------------------")

}
