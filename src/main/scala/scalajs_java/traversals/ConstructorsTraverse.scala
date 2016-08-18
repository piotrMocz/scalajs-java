package scalajs_java.traversals

import scalajs_java.trees._
import scalajs_java.utils.{ErrorHandler, Predicates}

class ConstructorsTraverse extends Traverse {

  var constructors: Map[String, List[MethodDecl]] = Map.empty

  var classes: List[ClassDecl] = Nil

  var anonClassCnt = 0

  def newAnonClassName(enclClassName: String): Name = {
    anonClassCnt += 1
    Name(enclClassName + "$$anon$" + anonClassCnt.toString)
  }

  def transformMethodDecls(classDecl: ClassDecl, newClassSym: Symbol): ClassDecl = {
    implicit val pos = classDecl.pos
    val newMembers = classDecl.members.map(transformMethodDecl(_, newClassSym))

    classDecl.copy(members = newMembers)
  }

  def transformMethodDecl(tree: Tree, newClassSym: Symbol): Tree = {
    implicit val pos = tree.pos
    tree match {
      case md: MethodDecl if Predicates.isConstructor(md) =>
        md.copy(symbol = newClassSym)

      case _ =>
        tree
    }
  }

  /** we need an additional method, because we need to do a few things after
    * the traversal is finished to collect the data
    */
  def traverseMain(compilationUnit: CompilationUnit): CompilationUnit = {
    implicit val pos = compilationUnit.pos
    val cu = traverse(compilationUnit)
    val newTypeDecls = cu.typeDecls ++ classes

    cu.copy(typeDecls = newTypeDecls)
  }

  override def traverse(classDecl: ClassDecl): ClassDecl = {
    implicit val pos = classDecl.pos
    val conss = classDecl.members.collect {
      case md: MethodDecl if md.symbol.isConstructor =>
        md
    }
    constructors += (classDecl.symbol.toString -> conss)

    super.traverse(classDecl)
  }

  override def traverse(newClass: NewClass): NewClass = {
    implicit val pos = newClass.pos

    newClass.classBody match {
      case None =>
        super.traverse(newClass)

      case Some(anonClass) =>
        newClass.ident match {
          case ident: Ident if ident.enclClass.isDefined =>
            val enclClass = ident.enclClass.get
            val anonName = newAnonClassName(enclClass)
            val ownerSymbol = new Symbol(anonName.str, ident.symbol)
            val newSymbol = ident.symbol.copy(name = anonName, owner = ownerSymbol, isConstructor = true)
            val newIdent = ident.copy(symbol = newSymbol, name = anonName)
            val newAnonClass = traverse(transformMethodDecls(anonClass, newSymbol))
                .copy(name = Name(anonName))(anonClass.pos)

            classes = newAnonClass :: classes

            NewClass(traverse(newIdent), newClass.typeArgs.map(traverse),
              newClass.args.map(traverse), Some(newAnonClass),
              newClass.enclExpr.map(traverse), newClass.tp)

          case _ =>
            throw new Exception("[ConstructorsTraverse -- NewClass]" +
                s"Cannot determine the enclosing class")

       }
  }
  }
}
