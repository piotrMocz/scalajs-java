package scalajs_java.compiler

import com.sun.tools.javac.code.TypeTag
import org.scalajs.core.ir.{Position, Trees => irt, Types => irtpe}

import scalajs_java.Config
import scalajs_java.compiler.passes.ConstructorPass.ConstructorsT
import scalajs_java.trees._
import scalajs_java.utils.Scope.ClassMapT
import scalajs_java.utils.{ErrorHandler, Fatal}

class Utils(val classes: ClassMapT,
            val errorHandler: ErrorHandler) {

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

  def argListsMatch(callArgs: List[Expr], defArgs: List[VarDecl]): Boolean = {

    if (callArgs.length != defArgs.length) return false
    if (callArgs.isEmpty) return true

    val callArgTypes = callArgs.map(_.tp)
    val defArgTypes = defArgs.map(vd => Type.getTypeFromTree(vd.varType))

    callArgTypes.zip(defArgTypes)
        .map(p => Type.eq(p._1, p._2))
        .reduce(_ && _)
  }

  /** Chooses a matching constructor for the NewClass call */
  def getMatchingConstructor(newClass: NewClass,
      constructors: ConstructorsT): MethodDecl = {
    implicit val pos = Utils.getPosition(newClass)

    val clsName = getClassNameFromTree(newClass.ident)

    val ctorCandidates = constructors.getOrElse(clsName,
      throw new Exception("No constructor found")) // TODO

    // find the first constructor, for which the argument list is ok:
    val ctor = ctorCandidates.find(md =>
      argListsMatch(newClass.args, md.params))

    ctor.getOrElse(throw new Exception("No constructor found"))
  }
}

object Utils {

  def getPosition(tree: Tree): Position =  tree.pos match {
    case scalajs_java.trees.Position(line) => Position(Position.SourceFile(Config.testFilePath), line, 0)
  }

  def adapt(tree: irt.Tree, targetType: irtpe.Type)(
    implicit pos: Position): irt.Tree = targetType match {
    case refTpe: irtpe.ReferenceType =>
      irt.AsInstanceOf(tree, refTpe)

    case _ =>
      irt.Unbox(tree, typeTag(targetType))
  }

  def typeTag(tpe: irtpe.Type): Char = tpe match {
    case irtpe.BooleanType => 'Z'
    case irtpe.DoubleType  => 'D'
    case irtpe.FloatType   => 'F'
    case irtpe.IntType     => 'I'
    case irtpe.LongType    => 'J'
    case _                 => throw new Exception(
      s"[typeTag] unknown tag")
  }

}