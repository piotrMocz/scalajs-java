package scalajs_java.trees

import javax.lang.model.`type`.TypeKind
import javax.lang.model.element.Modifier

import com.sun.tools.javac.code.Symbol.{ClassSymbol, MethodSymbol, VarSymbol}
import com.sun.tools.javac.code.{Symbol, TypeTag}
import com.sun.tools.javac.tree.JCTree.Tag
import com.sun.tools.javac.util.{Name => JName}

import scala.language.implicitConversions
import scalajs_java.utils.ScopeElem


// Tree

sealed trait Tree {
  implicit val pos: Position
}

case class CompilationUnit(imports: List[Import], typeDecls: List[Tree])(
    implicit val pos: Position) extends Tree

case class Import(qualifiedIdent: Tree)(
    implicit val pos: Position) extends Tree

case class Modifiers(flags: Set[Modifier], annotations: List[Annotation])(
    implicit val pos: Position) extends Tree

case class MethodDecl(name: Name, symbol: MethodSymbol, modifiers: Modifiers,
    typeParams: List[TypeParam], recvParam: Option[VarDecl],
    params: List[VarDecl], thrown: List[Expr], retType: Option[Tree],
    body: Block, defVal: Option[Expr])(implicit val pos: Position) extends Tree

case class TypeParam(name: Name, bounds: List[Expr],
    annotations: List[Annotation])(implicit val pos: Position) extends Tree

case class CatchTree(param: VarDecl, body: Block)(
    implicit val pos: Position) extends Tree

// Expressions

sealed trait Expr extends Tree with ExpressionTree

case class LetExpr(defs: List[VarDecl], expr: Tree, tp: Type)(
    implicit val pos: Position) extends Expr

// TODO attribute: Compound
case class Annotation(annotationType: Tree, args: List[Expr], tp: Type)(
    implicit val pos: Position) extends Expr

case class AnnotatedType(annotations: List[Annotation], underlyingType: Expr,
    tp: Type)(implicit val pos: Position) extends Expr

case class Wildcard(bound: Tree, tp: Type)(
    implicit val pos: Position) extends Expr

case class TypeIntersection(bounds: List[Expr], tp: Type)(
    implicit val pos: Position) extends Expr

case class TypeUnion(alternatives: List[Expr], tp: Type)(
    implicit val pos: Position) extends Expr

case class TypeApply(tpe: Tree, typeArgs: List[Expr], tp: Type)(
    implicit val pos: Position) extends Expr

case class ArrayTypeTree(elemType: Tree, tp: Type)(
    implicit val pos: Position) extends Expr

case class PrimitiveTypeTree(typeKind: TypeKind, typeTag: TypeTag, tp: Type)(
    implicit val pos: Position) extends Expr

// Literals

sealed trait Literal extends Expr

case class BooleanLiteral(value: Boolean, tp: Type)(
    implicit val pos: Position) extends Literal

case class CharLiteral(value: Char, tp: Type)(
    implicit val pos: Position) extends Literal

case class IntLiteral(value: Int, tp: Type)(
    implicit val pos: Position) extends Literal

case class LongLiteral(value: Long, tp: Type)(
    implicit val pos: Position) extends Literal

case class FloatLiteral(value: Float, tp: Type)(
    implicit val pos: Position) extends Literal

case class DoubleLiteral(value: Double, tp: Type)(
    implicit val pos: Position) extends Literal

case class ClassLiteral(value: Any, tp: Type)(
    implicit val pos: Position) extends Literal

case class NullLiteral()(
    implicit val pos: Position) extends Literal {
  val tp: Type = NullType
}

case class Ident(symbol: Symbol, name: Name, tp: Type,
    refVar: Option[ScopeElem]=None, enclClass: Option[String]=None)(
    implicit val pos: Position) extends Expr

case class FieldAccess(name: Name, symbol: Symbol, selected: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class ArrayAccess(indexed: Expr, index: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class InstanceOf(clazz: Tree, expr: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class TypeCast(clazz: Tree, expr: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class Binary(op: Tag, left: Expr, right: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class Unary(op: Tag, arg: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class AssignOp(variable: Expr, op: Tag, expr: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class Assign(variable: Expr, expr: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class Parens(expr: Expr, tp: Type)(
    implicit val pos: Position) extends Expr

case class NewArray(annotations: List[Annotation],
    dimAnnotations: List[List[Annotation]], dimensions: List[Expr],
    initializers: List[Expr], elemType: Option[Expr], tp: Type)(
    implicit val pos: Position) extends Expr


// Poly expressions

sealed trait PolyExpr extends Expr

case class MethodInv(methodSel: Expr, typeArgs: List[Expr], args: List[Expr],
    tp: Type, refDecl: Option[ScopeElem]=None)(
    implicit val pos: Position) extends PolyExpr

case class Conditional(cond: Expr, trueExpr: Expr, falseExpr: Expr, tp: Type)(
    implicit val pos: Position) extends PolyExpr

case class NewClass(ident: Expr, typeArgs: List[Expr], args: List[Expr],
    classBody: Option[ClassDecl], enclExpr: Option[Expr], tp: Type)(
    implicit val pos: Position) extends PolyExpr

// Functional expressions

sealed trait FuncExpr extends PolyExpr

case class MemberRef(name: Name, typeArgs: List[Expr], qualExpr: Expr,
    mode: ReferenceMode, polyKind: PolyKind, tp: Type)(
    implicit val pos: Position) extends FuncExpr

case class Lambda(params: List[VarDecl], body: Tree, bodyKind: BodyKind,
    tp: Type)(implicit val pos: Position) extends FuncExpr


// Statements

sealed trait Statement extends Tree with StatementTree

// TODO sym: Symbol.VarSymbol
case class VarDecl(mods: Modifiers, name: Name, nameExpr: Option[Expr],
    symbol: VarSymbol, varType: Tree, init: Option[Expr], kind: VarKind)(
    implicit val pos: Position) extends Statement

case class ClassDecl(name: Name, symbol: ClassSymbol, typeParams: List[TypeParam],
    extendsCl: Option[Expr], implementsCl: List[Expr], members: List[Tree])(
    implicit val pos: Position) extends Statement

case class Assert(cond: Expr, detail: Expr)(
    implicit val pos: Position) extends Statement

case class Throw(expr: Expr)(implicit val pos: Position) extends Statement

case class Return(expr: Option[Expr])(implicit val pos: Position) extends Statement

case class Continue(label: Option[Name])(
    implicit val pos: Position) extends Statement

case class Break(label: Option[Name])(
    implicit val pos: Position) extends Statement

case class ExprStatement(expr: Expr)(
    implicit val pos: Position) extends Statement

case class If(cond: Expr, thenStmt: Statement, elseStmt: Option[Statement])(
    implicit val pos: Position) extends Statement

case class Block(statements: List[Statement], isStatic: Boolean)(
    implicit val pos: Position) extends Statement

case class TryStmt(resources: List[Tree], body: Block,
    catches: List[CatchTree], finallyBlk: Option[Block])(
    implicit val pos: Position) extends Statement

case class Skip(implicit val pos: Position) extends Statement

case class Case(pat: Expr, statements: List[Statement])(
    implicit val pos: Position) extends Statement

case class Switch(selector: Expr, cases: List[Case])(
    implicit val pos: Position) extends Statement

case class Synchronized(lock: Expr, body: Block)(
    implicit val pos: Position) extends Statement

case class LabeledStmt(label: Name, body: Statement)(
    implicit val pos: Position) extends Statement

case class EnhancedForLoop(variable: VarDecl, expr: Expr, body: Statement)(
    implicit val pos: Position) extends Statement

case class ForLoop(init: List[Statement], cond: Option[Expr],
    update: List[ExprStatement], body: Statement)(
    implicit val pos: Position) extends Statement

case class WhileLoop(cond: Expr, body: Statement)(
    implicit val pos: Position) extends Statement

case class DoWhileLoop(cond: Expr, body: Statement)(
    implicit val pos: Position) extends Statement


// Other

case class ErrorTree(pos: Position) extends Tree with Expr with Statement

case class Name(str: String)

case object Name {
  def fromJName(jname: JName) = Name(jname.toString)

  implicit def nameToString(name: Name): String =
    name.str
}

case class Position(line: Int)
case object Position {
  def noPosition = Position(0)
}

sealed trait PolyKind
case object Standalone extends PolyKind
case object Poly extends PolyKind


sealed trait ReferenceMode
case object Invoke extends ReferenceMode
case object New extends ReferenceMode


sealed trait BodyKind
case object StatementKind extends BodyKind
case object ExpressionKind extends BodyKind

sealed trait VarKind
case object ClassMember extends VarKind
case object Param extends VarKind
case object LocalVar extends VarKind
case object Method extends VarKind