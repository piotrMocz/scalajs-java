package scalajs_java.traversals

import com.sun.tools.javac.code.Symbol.TypeSymbol
import com.sun.tools.javac.code.Type.JCPrimitiveType
import com.sun.tools.javac.code.TypeTag
import com.sun.tools.javac.tree.JCTree.Tag

import scalajs_java.trees._

/** Transorms Java-specific operations to their Scala equivalents */
class OperationsTraverse extends Traverse {

  def convertASGtoOp(op: Tag): Tag = op match {
    case Tag.BITOR_ASG  => Tag.BITOR
    case Tag.BITXOR_ASG => Tag.BITXOR
    case Tag.BITAND_ASG => Tag.BITAND
    case Tag.SL_ASG     => Tag.SL
    case Tag.SR_ASG     => Tag.SR
    case Tag.USR_ASG    => Tag.USR
    case Tag.PLUS_ASG   => Tag.PLUS
    case Tag.MINUS_ASG  => Tag.MINUS
    case Tag.MUL_ASG    => Tag.MUL
    case Tag.DIV_ASG    => Tag.DIV
    case Tag.MOD_ASG    => Tag.MOD
    case _              => throw new Exception(
      s"[OperationsTraverse.convertASGtoOp] Op $op is not an assign-op.")
  }

  override def traverse(assignOp: AssignOp): Assign = {
    implicit val pos = assignOp.pos

    val newOp = convertASGtoOp(assignOp.op)
    val binop = Binary(newOp, assignOp.variable,
      super.traverse(assignOp.expr), assignOp.tp)

    Assign(super.traverse(assignOp.variable), binop, assignOp.tp)
  }

  override def traverse(forLoop: ForLoop): Statement = {
    implicit val pos = forLoop.pos

    val cond = forLoop.cond.getOrElse(
      BooleanLiteral(true, JExprType(new JCPrimitiveType(TypeTag.BOOLEAN, null))))
    val body = Block(traverse(forLoop.body) :: forLoop.update, isStatic = false)
    val loop = WhileLoop(cond, body)

    Block(forLoop.init :+ loop, isStatic = false)
  }

  override def traverse(doWhileLoop: DoWhileLoop): Statement = {
    implicit val pos = doWhileLoop.pos
    val body = traverse(doWhileLoop.body)
    val whileLoop = WhileLoop(doWhileLoop.cond, body)

    Block(List(body, whileLoop), isStatic = false)
  }

}
