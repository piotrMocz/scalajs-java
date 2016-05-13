
object Main extends App {

  val compiler = new CompilerInterface()
  compiler.compile("Test2.java")

  println(compiler.compilationUnit.getImports)
  println(compiler.compilationUnit.getPackageName)


  println("------------------------- Scala AST ----------------------")
  val tree = TreeTraverse.traverse(compiler.compilationUnit)
  println(tree.toString)
  println("\n\n")

  println("---------------------------- AST -------------------------")
  val treeVisitor = new JTreeVisitor(false)
  compiler.compilationUnit.getTree.accept(treeVisitor)
  println("\n\n")

  println("---------------------------- ENV -------------------------")
  compiler.printEnvs()

}
