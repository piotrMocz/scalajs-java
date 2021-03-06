package scalajs_java.compiler

import com.sun.tools.javac.code.{TypeTag, Type => JType}
import org.scalajs.core.ir.{Types => irtpe, Position}

import scalajs_java.trees.{ArrayTypeTree, Ident, _}
import scalajs_java.utils._

/** Compilation of types only.
  *
  * The methods return instances of `irtpe.Type`
  */
class TypeCompiler(mangler: Mangler, errorHanlder: ErrorHandler) {

  def isArrayType(jExprType: JExprType): Boolean =
    jExprType.jtype.getTag == TypeTag.ARRAY

  def isArrayType(typeTree: Tree): Boolean = typeTree match {
    case _: ArrayTypeTree => true
    case _                => false
  }

  def compilePrimitiveType(tTag: TypeTag)(
      implicit pos: Position): irtpe.Type = tTag match {
    case TypeTag.BOOLEAN => irtpe.BooleanType
    case TypeTag.BYTE    => irtpe.IntType
    case TypeTag.CHAR    => irtpe.IntType
    case TypeTag.DOUBLE  => irtpe.DoubleType
    case TypeTag.FLOAT   => irtpe.FloatType
    case TypeTag.INT     => irtpe.IntType
    case TypeTag.LONG    => irtpe.LongType
    case TypeTag.SHORT   => irtpe.IntType
    case TypeTag.VOID    => irtpe.NoType
    case _               => errorHanlder.fail(pos.line,
      Some("compilePrimitiveType"), s"Not a primitive type: $tTag", Normal)
      irtpe.NoType
  }

  def compileClassType(tpe: JType): irtpe.Type =
    mangler.encodeClassType(Symbol.fromJava(tpe.tsym))

  def compileArrayType(tpe: JType)(implicit pos: Position): irtpe.Type = {
    val tTag = mangler.arrayTypeTag(tpe.toString)
    val dims = getArrayDims(tpe)
    irtpe.ArrayType(tTag, dims)
  }

  def compileAutoboxedType(tpe: Type): irtpe.Type = tpe match {
    case JExprType(jtype) => jtype.tsym.toString match {
      case "java.lang.Boolean" =>
        irtpe.BooleanType

      case "java.lang.Char"    | "java.lang.Byte" |
           "java.lang.Integer" | "java.lang.Short" =>
        irtpe.IntType

      case "java.lang.Float" =>
        irtpe.FloatType

      case "java.lang.Double" =>
        irtpe.DoubleType

      case "java.lang.Long" =>
        irtpe.LongType

      case _ =>
        errorHanlder.fail(0, Some("compileAutoboxedType"),
          s"Unreconized java type: $jtype", Fatal)
        irtpe.NoType
    }

    case _ =>
      errorHanlder.fail(0, Some("compileAutoboxedType"),
        s"Unreconized type: $tpe", Fatal)
      irtpe.NoType
  }

  def compileJavaType(tpe: JExprType)(implicit pos: Position): irtpe.Type = {
    if (tpe.jtype.isPrimitiveOrVoid) compilePrimitiveType(tpe.jtype.getTag)
    else if (isArrayType(tpe)) compileArrayType(tpe.jtype)
    else if (Predicates.isTypeParameter(tpe)) irtpe.AnyType
    else if (Predicates.isErasedParameter(tpe)) irtpe.AnyType
    else if (Predicates.isStringType(tpe)) irtpe.ClassType("T") // irtpe.StringType
    else if (Predicates.isAutoboxedType(tpe)) compileAutoboxedType(tpe)
    else compileClassType(tpe.jtype)
  }

  /** Compile a type encoded as an AST attribute */
  def compileType(tpe: Type)(implicit pos: Position): irtpe.Type = {
    tpe match {
      case tp: JExprType            => compileJavaType(tp)
      case AnyType                  => irtpe.AnyType
      case StatementType | NullType => irtpe.NoType
    }
  }

  def getArrayDims(tpe: JType)(implicit pos: Position): Int = {
    if (tpe.toString.endsWith("[][][][][]")) 5
    else if (tpe.toString.endsWith("[][][][]")) 4
    else if (tpe.toString.endsWith("[][][]")) 3
    else if (tpe.toString.endsWith("[][]")) 2
    else if (tpe.toString.endsWith("[]")) 1
    else {
      errorHanlder.fail(pos.line, Some("getArrayDims"),
          "Can only compile arrays up to 5 dimenstions", Normal)
      0
    }
  }

  def getArrayDims(typeTree: Tree)(implicit pos: Position): Int = {
    typeTree match {
      case ArrayTypeTree(tree, _) => 1 + getArrayDims(tree)
      case _ => 0
    }
  }

  def getArrayElemType(typeTree: Tree): Tree = typeTree match {
    case ArrayTypeTree(tree, _) => getArrayElemType(tree)
    case tree                   => tree
  }

  def compileClassType(typeTree: Tree)(
      implicit pos: Position): irtpe.ClassType = typeTree match {
    case id: Ident =>
      mangler.encodeClassType(id.symbol)

    case fa: FieldAccess =>
      mangler.encodeClassType(fa.symbol)

    case ta: TypeApply =>
      compileClassType(ta.tpe)

    case _ =>
      errorHanlder.fail(pos.line, Some("compileClassType"),
        s"[compileClassType] Not a class type tree: ${typeTree.toString}",
        Fatal)
      irtpe.ClassType("")
  }

  /** Compile a type encoded as an AST node */
  def compileType(tpe: Tree)(implicit pos: Position): irtpe.Type = {
    tpe match {
      case t: TypedTree if Predicates.isTypeParameter(t.tp) =>
        irtpe.AnyType

      case t: TypedTree if Predicates.isAutoboxedType(t.tp) =>
        compileAutoboxedType(t.tp)

      case PrimitiveTypeTree(_, tTag, _) =>
        compilePrimitiveType(tTag)

      case aType@ArrayTypeTree(elemType, _) =>
        val dims = getArrayDims(aType)
        val tname = mangler.mangleType(getArrayElemType(elemType))
        irtpe.ArrayType(tname, dims)

      case ident: Ident =>
        if (ident.symbol.toString == "java.lang.String") irtpe.ClassType("T")// irtpe.StringType
        else compileClassType(ident)  // TODO there're more cases, I guess

      case fa: FieldAccess =>
        compileClassType(fa)

      case ta: TypeApply =>
        compileClassType(ta.tpe)

      case _ =>
        errorHanlder.fail(pos.line, Some("compileType"),
          s"Missing implementation (trying to compile: $tpe)", Fatal)
        null
    }
  }
}
