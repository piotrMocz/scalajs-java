package trees

import com.sun.tools.javac.code.{Symbol, TypeTag}
import com.sun.tools.javac.util.{Name => JName}
import javax.lang.model.element.Modifier
import javax.lang.model.`type`.TypeKind

import com.sun.tools.javac.code.Symbol.MethodSymbol


/* --------------------------------------------------------------------------------------------
 * ------- Tree -------------------------------------------------------------------------------
 * -------------------------------------------------------------------------------------------- */
sealed trait Tree

case class CompilationUnit(imports: List[Import],
                           typeDecls: List[Tree]) extends Tree


case class Import(qualifiedIdent: Tree) extends Tree


case class Modifiers(flags: Set[Modifier],
                     annotations: List[Annotation]) extends Tree


case class MethodDecl(name: Name,
                      symbol: MethodSymbol,
                      modifiers: Modifiers,
                      typeParams: List[TypeParam],
                      recvParam: Option[VarDecl],
                      params: List[VarDecl],
                      thrown: List[Expr],
                      retType: Option[Tree],
                      body: Block,
                      defVal: Option[Expr]) extends Tree


case class TypeParam(name: Name,
                     bounds: List[Expr],
                     annotations: List[Annotation]) extends Tree


case class CatchTree(param: VarDecl,
                     body: Block) extends Tree


/* --------------------------------------------------------------------------------------------
 * ------- Expressions ------------------------------------------------------------------------
 * -------------------------------------------------------------------------------------------- */
sealed trait Expr extends Tree


case class LetExpr(defs: List[VarDecl],
                   expr: Tree) extends Expr


case class Annotation(annotationType: Tree,
                      args: List[Expr] /* TODO attribute: Compound */) extends Expr


case class Erroneous(trees: List[Tree]) extends Expr


case class AnnotatedType(annotations: List[Annotation],
                         underlyingType: Expr) extends Expr


case class Wildcard(bound: Tree) extends Expr


case class TypeIntersection(bounds: List[Expr]) extends Expr


case class TypeUnion(alternatives: List[Expr]) extends Expr


case class TypeApply(tpe: Tree,
                     typeArgs: List[Expr]) extends Expr


case class ArrayTypeTree(elemType: Tree) extends Expr


case class PrimitiveTypeTree(typeKind: TypeKind,
                             typeTag: TypeTag) extends Expr


case class Literal(typeTag: TypeTag,
                   value: AnyRef) extends Expr


case class Ident(symbol: Symbol,
                 name: Name) extends Expr


case class FieldAccess(name: Name,
                       symbol: Symbol,
                       selected: Expr) extends Expr


case class ArrayAccess(indexed: Expr,
                       index: Expr) extends Expr


case class InstanceOf(clazz: Tree,
                      expr: Expr) extends Expr


case class TypeCast(clazz: Tree,
                    expr: Expr) extends Expr


case class Binary(op: Symbol,
                  left: Expr,
                  right: Expr) extends Expr


case class Unary(op: Symbol,
                 arg: Expr) extends Expr


case class AssignOp(variable: Expr,
                    op: Symbol,
                    expr: Expr) extends Expr


case class Assign(variable: Expr,
                  expr: Expr) extends Expr


case class Parens(expr: Expr) extends Expr


case class NewArray(annotations: List[Annotation],
                    dimAnnotations: List[List[Annotation]],
                    dimensions: List[Expr],
                    initializers: List[Expr],
                    elemType: Option[Expr]) extends Expr


/* --------------------------------------------------------------------------------------------
 * ------- Poly expressions -------------------------------------------------------------------
 * -------------------------------------------------------------------------------------------- */
sealed trait PolyExpr extends Expr


case class MethodInv(methodSel: Expr,
                     typeArgs: List[Expr],
                     args: List[Expr]) extends PolyExpr


case class Conditional(cond: Expr,
                       trueExpr: Expr,
                       falseExpr: Expr) extends PolyExpr


case class NewClass(ident: Expr,
                    typeArgs: List[Expr],
                    args: List[Expr],
                    classBody: Option[ClassDecl],
                    enclExpr: Option[Expr]) extends PolyExpr

/* --------------------------------------------------------------------------------------------
 * ------- Functional expressions -------------------------------------------------------------
 * -------------------------------------------------------------------------------------------- */
sealed trait FuncExpr extends PolyExpr


case class MemberRef(name: Name,
                     typeArgs: List[Expr],
                     qualExpr: Expr,
                     mode: ReferenceMode,
                     polyKind: PolyKind) extends FuncExpr


case class Lambda(params: List[VarDecl],
                  body: Tree,
                  bodyKind: BodyKind) extends FuncExpr


/* --------------------------------------------------------------------------------------------
 * ------- Statements -------------------------------------------------------------------------
 * -------------------------------------------------------------------------------------------- */
sealed trait Statement extends Tree


case class VarDecl(mods: Modifiers,
                   name: Name,
                   nameExpr: Option[Expr],
                   varType: Tree,
                   init: Option[Expr] /* TODO sym: Symbol.VarSymbol */) extends Statement


case class ClassDecl(typeParams: List[TypeParam],
                     extendsCl: Option[Expr],
                     implementsCl: List[Expr],
                     members: List[Tree]) extends Statement


case class Assert(cond: Expr,
                  detail: Expr) extends Statement


case class Throw(expr: Expr) extends Statement


case class Return(expr: Expr) extends Statement


case class Continue(label: Option[Name],
                    target: Option[Tree]) extends Statement


case class Break(label: Option[Name],
                 target: Option[Tree]) extends Statement


case class ExprStatement(expr: Expr) extends Statement


case class If(cond: Expr,
              thenStmt: Statement,
              elseStmt: Option[Statement]) extends Statement


case class Block(statements: List[Statement],
                 isStatic: Boolean) extends Statement


case class TryStmt(resources: List[Tree],
                   body: Block,
                   catches: List[CatchTree],
                   finallyBlk: Option[Block]) extends Statement


case object Skip extends Statement


case class Case(pat: Expr,
                statements: List[Statement]) extends Statement


case class Switch(selector: Expr,
                  cases: List[Case]) extends Statement


case class Synchronized(lock: Expr,
                        body: Block) extends Statement


case class LabeledStmt(label: Name,
                       body: Statement) extends Statement


case class EnhancedForLoop(variable: VarDecl,
                           expr: Expr,
                           body: Statement) extends Statement


case class ForLoop(init: List[Statement],
                   cond: Option[Expr],
                   update: List[ExprStatement],
                   body: Statement) extends Statement


case class WhileLoop(cond: Expr,
                     body: Statement) extends Statement


case class DoWhileLoop(cond: Expr,
                       body: Statement) extends Statement


/* --------------------------------------------------------------------------------------------
 * ------- Other ------------------------------------------------------------------------------
 * -------------------------------------------------------------------------------------------- */
case class Name(str: String)

case object Name {
  def fromJName(jname: JName) = Name(jname.toString)
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

