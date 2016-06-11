package scalajs_java.trees

import com.sun.tools.javac.code.{Type=>JType}

sealed trait Type

/*
 *   Blank type for statements
 */
case object StatementType extends Type

case class JExprType(jtype: JType) extends Type

/*
 *   Primitive types
 */
//sealed trait PrimitiveType extends Type
//
//case object BoolType extends PrimitiveType
//
//case object CharType extends PrimitiveType
//
//case object IntType extends PrimitiveType
//
//case object LongType extends PrimitiveType
//
//case object FloatType extends PrimitiveType
//
//case object DoubleType extends PrimitiveType
//
//
///*
// *   Object type
// */
//sealed trait ObjectType extends Type

trait TypedTree {
  def tp: Type
}

trait StatementTree extends TypedTree {
  override def tp: Type = StatementType
}

trait ExpressionTree extends TypedTree
