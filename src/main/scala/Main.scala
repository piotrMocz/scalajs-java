
object Main extends App {

  val compiler = new CompilerInterface()
  compiler.compile("Test.java")

  println(compiler.compilationUnit.getImports)
  println(compiler.compilationUnit.getPackageName)

  val tree = TreeTraverse.traverse(compiler.compilationUnit)

  println("---------------------------- AST -------------------------")
  val treeVisitor = new JTreeVisitor()
  compiler.compilationUnit.getTree.accept(treeVisitor)
  println("----------------------------------------------------------")
}
