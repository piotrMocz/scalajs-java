package scalajs_java.compiler

import org.scalajs.core.ir.{Position, Types=>irtpe}

import scalajs_java.Config
import scalajs_java.compiler.passes.ConstructorPass.ConstructorsT
import scalajs_java.trees._
import scalajs_java.utils.Scope.ClassMapT
import scalajs_java.utils.{ErrorHandler, Fatal, Predicates}

class Utils(val classes: ClassMapT,
            val errorHandler: ErrorHandler) {

  def getPosition(tree: Tree): Position =  tree.pos match {
    case scalajs_java.trees.Position(line) => Position(Position.SourceFile(Config.testFilePath), line, 0)
  }

  def getClassNameFromTree(tree: Tree)(
      implicit pos: Position): String = tree match {
    case ident: Ident =>
      ident.symbol.toString

    case fieldAccess: FieldAccess =>
      fieldAccess.symbol.toString

    case typeApply: TypeApply =>
      getClassNameFromTree(typeApply.tpe)

    case _ =>
      errorHandler.fail(pos.line, Some("getClassNameFromExpr"),
        s"Failed to determine class name (from $tree)", Fatal)
      ""
  }

  def getParamTypesFromTree(tree: Tree)(
      implicit pos: Position): List[Type] = tree match {
    case typeApply: TypeApply =>
      typeApply.typeArgs.map(Type.getTypeFromTree)

    case _ =>
      Nil
  }

  /** Gets the bindings like:
    * (generic type name -> type it was instantiated to) */
  def getTypeParamBindings(newClass: NewClass): Map[String, Type] = {
    implicit val pos = getPosition(newClass)
    val className = getClassNameFromTree(newClass.ident)
    classes.get(className) match {
      case Some(classDecl) =>
        val typeParNames = classDecl.typeParams.map(_.name.str)
        val appliedTypes = getParamTypesFromTree(newClass.ident)
        typeParNames.zip(appliedTypes).toMap

      case None =>
        errorHandler.fail(pos.line, Some("getParamTypeBindings"),
          "Failed to get the param type bindings for class", Fatal)
        Map.empty
    }
  }

  def argListsMatch(callArgs: List[Expr], typesMap: Map[String, Type], defArgs: List[VarDecl]): Boolean = {
    if (callArgs.length != defArgs.length) return false
    if (callArgs.isEmpty) return true

    val defArgTypes = defArgs.map(tp => tp.varType match {
      case id: Ident if typesMap.contains(id.name.str) =>
        typesMap(id.name.str) // TODO error handling

      case tpTree =>
        Type.getTypeFromTree(tpTree)
    })

    val callArgTypes = callArgs.map(_.tp)

    (callArgTypes zip defArgTypes).map(p => Type.eq(p._1, p._2)).reduce {_ && _}
  }

  /** Chooses a matching constructor for the NewClass call */
  def getMatchingConstructor(newClass: NewClass,
      constructors: ConstructorsT): MethodDecl = {
    implicit val pos = getPosition(newClass)

    val clsName = getClassNameFromTree(newClass.ident)

    val ctorCandidates = constructors.getOrElse(clsName,
      throw new Exception("No constructor found")) // TODO
    val tParamBindings = getTypeParamBindings(newClass)

    // find the first constructor, for which the argument list is ok:
    val ctor = ctorCandidates.find(md => argListsMatch(newClass.args,
      tParamBindings, md.params))

    ctor.getOrElse(throw new Exception("No constructor found"))
  }
}
