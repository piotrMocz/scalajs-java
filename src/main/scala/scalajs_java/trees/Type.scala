package scalajs_java.trees

import com.sun.tools.javac.code.Type.JCPrimitiveType
import com.sun.tools.javac.code.{TypeTag, Type => JType}

sealed trait Type

/*
 *   Blank type for statements
 */
case object StatementType extends Type

case class JExprType(jtype: JType) extends Type

case object NoType extends Type

case object JExprType {
  def booleanType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.BOOLEAN, null))

  def charType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.CHAR, null))

  def intType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.INT, null))

  def longType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.LONG, null))

  def floatType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.FLOAT, null))

  def doubleType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.DOUBLE, null))
}

trait TypedTree {
  def tp: Type
}

trait StatementTree extends TypedTree {
  override def tp: Type = StatementType
}

trait ExpressionTree extends TypedTree
