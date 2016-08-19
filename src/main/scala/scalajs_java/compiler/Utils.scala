package scalajs_java.compiler

import com.sun.tools.javac.code.TypeTag
import org.scalajs.core.ir.{Position, Types => irtpe}

import scalajs_java.compiler.passes.ConstructorPass.ConstructorsT
import scalajs_java.runtime.Config
import scalajs_java.trees._
import scalajs_java.utils.scope.Scope.ClassMapT
import scalajs_java.utils.{ErrorHandler, Fatal, Predicates}

class Utils(val classes: ClassMapT,
            val errorHandler: ErrorHandler) {

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
      constructors: ConstructorsT): Option[MethodDecl] = {
    implicit val pos = Utils.getPosition(newClass)

    val clsName = getClassNameFromTree(newClass.ident)

    val ctorCandidates = newClass.classBody match {
      case Some(classDecl) =>
        classDecl.members.collect {
          case md: MethodDecl if Predicates.isConstructor(md) => md
        }

      case None =>
        constructors.getOrElse(clsName,
          throw new Exception("No constructor found"))
    }

    // find the first constructor, for which the argument list is ok:
    val ctor = ctorCandidates.find(md =>
      argListsMatch(newClass.args, md.params))

    ctor
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
}

object Utils {

  def getPosition(tree: Tree): Position =  tree.pos match {
    case scalajs_java.trees.Position(line) => Position(Position.SourceFile(Config.testFilePath), line, 0)
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

  def getJavaTypeTag(tpe: Type): TypeTag = tpe match {
    case JExprType(jtype) if Predicates.isAutoboxedType(tpe) =>
      jtype.tsym.toString match {
        case "java.lang.Boolean" =>
          TypeTag.BOOLEAN

        case "java.lang.Char" =>
          TypeTag.CHAR

        case "java.lang.Byte" => TypeTag.BYTE

        case "java.lang.Integer" => TypeTag.INT

        case "java.lang.Short" => TypeTag.SHORT

        case "java.lang.Float" => TypeTag.FLOAT

        case "java.lang.Double" => TypeTag.DOUBLE

        case "java.lang.Long" => TypeTag.LONG

        case _ => throw new Exception(
          s"No type tag for type: $jtype")
      }

    case JExprType(jtype) =>
      jtype.getTag

    case _ => throw new Exception(
      s"Type $tpe is not an autoboxed type")
  }
}