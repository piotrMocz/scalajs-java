package scalajs_java.traversals

import com.sun.tools.javac.code.Type.JCPrimitiveType
import com.sun.tools.javac.code.TypeTag
import com.sun.tools.javac.tree.JCTree.Tag

import scalajs_java.trees._
import scalajs_java.utils.ErrorHandler
import scalajs_java.utils.scope.Scope.ClassMapT

/** Transorms Java-specific operations to their Scala equivalents */
class DesugarTraverse(val errorHanlder: ErrorHandler, val classes: ClassMapT) extends Traverse {

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

  def mkInitMethod(params: List[VarDecl])(
      implicit pos: Position): MethodDecl = {
    val initName = Name("<init>")
    val initSymbol = Symbol("<init>", null, isConstructor = true)
    val emptyMods = Modifiers(Set.empty, Nil)
    val emptyBlock = Block(Nil, isStatic = false)

    MethodDecl(initName, initSymbol, emptyMods, Nil,
      None, params, Nil, None, emptyBlock, None)
  }

  override def traverse(assignOp: AssignOp): Assign = {
    implicit val pos: Position = assignOp.pos

    val newOp = convertASGtoOp(assignOp.op)
    val binop = Binary(newOp, assignOp.variable,
      super.traverse(assignOp.expr), assignOp.tp)

    Assign(super.traverse(assignOp.variable), binop, assignOp.tp)
  }

  override def traverse(forLoop: ForLoop): Statement = {
    implicit val pos: Position = forLoop.pos

    val cond = forLoop.cond.getOrElse(
      BooleanLiteral(value = true, JExprType(new JCPrimitiveType(TypeTag.BOOLEAN, null))))
    val body = Block(traverse(forLoop.body) :: forLoop.update, isStatic = false)
    val loop = WhileLoop(cond, body)

    Block(forLoop.init :+ loop, isStatic = false)
  }

  override def traverse(lambda: Lambda): NewClass = {
    implicit val pos: Position = lambda.pos

    lambda.tp match {
      case tp@JExprType(jtype) =>
        val sym = Symbol.fromJava(jtype.tsym).copy(isInterface = false)
        val clsName = jtype.tsym.toString
        val newClassName = Name(sym.name.split('.').toList.last)
        val newClassIdent = Ident(sym, newClassName, tp)
        val ctor = mkInitMethod(Nil)
        val ancestorIdent = Ident(sym, Name(clsName), tp)
        val (extendCl, implementCl) =
          if (jtype.tsym.isInterface) (None, List(ancestorIdent))
          else (Some(ancestorIdent), Nil)

        val baseClass = classes
            .getOrElse(clsName, throw new Exception("Couldn't find base class"))

        // this is actually quite safe, because if the
        // class wasn't a functional interface, javac's TC
        // would throw an error earlier:
        val implMethod = baseClass.members
            .head.asInstanceOf[MethodDecl]
            .copy(body = lambda.body)

        val classDecl = ClassDecl(
          Name(clsName),
          sym,
          Nil,
          extendCl,
          implementCl,
          members = List(ctor, implMethod))

        NewClass(newClassIdent, Nil, Nil, Some(classDecl), None, lambda.tp)

      case tp =>
        throw new Exception(s"Unexpected lambda type: $tp")
    }
  }

}
