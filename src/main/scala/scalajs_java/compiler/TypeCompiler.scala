package scalajs_java.compiler

import com.sun.tools.javac.code.{Type => JType, TypeTag}
import org.scalajs.core.ir.{Types => irtpe}

import scalajs_java.trees.{ArrayTypeTree, Ident, _}
import scalajs_java.utils.Mangler

/** Compilation of types only.
  *
  * The methods return instances of `irtpe.Type`
  */
object TypeCompiler {

  def compilePrimitiveType(tTag: TypeTag): irtpe.Type = tTag match {
    case TypeTag.BOOLEAN => irtpe.BooleanType
    case TypeTag.BYTE    => irtpe.IntType
    case TypeTag.CHAR    => irtpe.IntType
    case TypeTag.DOUBLE  => irtpe.DoubleType
    case TypeTag.FLOAT   => irtpe.FloatType
    case TypeTag.INT     => irtpe.IntType
    case TypeTag.LONG    => irtpe.LongType
    case TypeTag.SHORT   => irtpe.IntType
    case TypeTag.VOID    => irtpe.NoType
    case _               => throw new Exception(
      "[compilePrimitiveType] Not a primitive type")
  }

  def compileObjectType(tpe: JType): irtpe.Type = ???  // TODO

  def compileJavaType(tpe: JExprType): irtpe.Type = {
    if (tpe.jtype.isPrimitive) compilePrimitiveType(tpe.jtype.getTag)
    else compileObjectType(tpe.jtype)
  }

  /** Compile a type encoded as an AST attribute */
  def compileType(tpe: Type): irtpe.Type = tpe match {
    case tp: JExprType => compileJavaType(tp)
    case StatementType => irtpe.NoType
  }

  /** Compile a type encoded as an AST node */
  def compileType(tpe: Tree): irtpe.Type = tpe match {
    case PrimitiveTypeTree(_, tTag, _) =>
      compilePrimitiveType(tTag)

    case ArrayTypeTree(elemType, _) =>
      val tname = Mangler.mangleType(elemType)
      irtpe.ArrayType(tname, 1)

    case Ident(sym, _, _, _) =>
      if (sym.toString == "java.lang.String") irtpe.StringType
      else throw new Exception("[compileType] Cannot yet compile this type.") // TODO compile object types

    case _ =>
      ???
  }

}
