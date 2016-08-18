package scalajs_java.traversals

import scalajs_java.trees._

/** Generic traversal of a `Tree`.
  *
  * To create a specific traversal just extend this trait and override
  * the methods you need. */
trait Traverse {

  def traverse(compilationUnit: CompilationUnit): CompilationUnit = {
    CompilationUnit(compilationUnit.imports.map(traverse),
      compilationUnit.typeDecls.map(traverse))(compilationUnit.pos)
  }

  def traverse(imp: Import): Import = {
    Import(traverse(imp.qualifiedIdent))(imp.pos)
  }

  def traverse(methodDecl: MethodDecl): MethodDecl = {
    MethodDecl(methodDecl.name, methodDecl.symbol,
      traverse(methodDecl.modifiers).asInstanceOf[Modifiers],
      methodDecl.typeParams.map(traverse), methodDecl.recvParam.map(traverse),
      methodDecl.params.map(traverse), methodDecl.thrown.map(traverse),
      methodDecl.retType.map(traverse), traverse(methodDecl.body),
      methodDecl.defVal.map(traverse))(methodDecl.pos)
  }

  def traverse(typeParam: TypeParam): TypeParam = {
    TypeParam(typeParam.name, typeParam.bounds.map(traverse),
      typeParam.annotations.map(traverse))(typeParam.pos)
  }

  def traverse(catchTree: CatchTree): CatchTree = {
    CatchTree(traverse(catchTree.param), traverse(catchTree.body))(
      catchTree.pos)
  }

  def traverse(letExpr: LetExpr): LetExpr = {
    LetExpr(letExpr.defs.map(traverse), traverse(letExpr.expr), letExpr.tp)(
      letExpr.pos)
  }

  def traverse(annotation: Annotation): Annotation = {
    Annotation(traverse(annotation.annotationType),
      annotation.args.map(traverse), annotation.tp)(annotation.pos)
  }

  def traverse(annotatedType: AnnotatedType): AnnotatedType = {
    AnnotatedType(annotatedType.annotations.map(traverse),
      traverse(annotatedType.underlyingType), annotatedType.tp)(
      annotatedType.pos)
  }

  def traverse(wildcard: Wildcard): Wildcard = {
    Wildcard(traverse(wildcard.bound), wildcard.tp)(wildcard.pos)
  }

  def traverse(typeIntersection: TypeIntersection): TypeIntersection = {
    TypeIntersection(typeIntersection.bounds.map(traverse),
      typeIntersection.tp)(typeIntersection.pos)
  }

  def traverse(typeUnion: TypeUnion): TypeUnion = {
    TypeUnion(typeUnion.alternatives.map(traverse), typeUnion.tp)(
      typeUnion.pos)
  }

  def traverse(typeApply: TypeApply): TypeApply = {
    TypeApply(traverse(typeApply.tpe), typeApply.typeArgs.map(traverse),
      typeApply.tp)(typeApply.pos)
  }

  def traverse(arrayTypeTree: ArrayTypeTree): ArrayTypeTree = {
    ArrayTypeTree(traverse(arrayTypeTree.elemType), arrayTypeTree.tp)(
      arrayTypeTree.pos)
  }

  def traverse(classLiteral: ClassLiteral): ClassLiteral = classLiteral

  def traverse(literal: Literal): Literal = literal

  def traverse(ident: Ident): Ident = ident

  def traverse(fieldAccess: FieldAccess): FieldAccess = {
    FieldAccess(fieldAccess.name, fieldAccess.symbol,
      traverse(fieldAccess.selected), fieldAccess.tp)(fieldAccess.pos)
  }

  def traverse(arrayAccess: ArrayAccess): ArrayAccess = {
    ArrayAccess(traverse(arrayAccess.indexed), traverse(arrayAccess.index),
      arrayAccess.tp)(arrayAccess.pos)
  }

  def traverse(instanceOf: InstanceOf): InstanceOf = {
    InstanceOf(traverse(instanceOf.clazz), traverse(instanceOf.expr),
      instanceOf.tp)(instanceOf.pos)
  }

  def traverse(typeCast: TypeCast): TypeCast = {
    TypeCast(traverse(typeCast.clazz), traverse(typeCast.expr), typeCast.tp)(
      typeCast.pos)
  }

  def traverse(binary: Binary): Binary = {
    Binary(binary.op, traverse(binary.left), traverse(binary.right),
      binary.tp)(binary.pos)
  }

  def traverse(unary: Unary): Expr = {
    Unary(unary.op, traverse(unary.arg), unary.tp)(unary.pos)
  }

  def traverse(assignOp: AssignOp): Expr = {
    AssignOp(traverse(assignOp.variable), assignOp.op,
      traverse(assignOp.expr), assignOp.tp)(assignOp.pos)
  }

  def traverse(assign: Assign): Assign = {
    Assign(traverse(assign.variable), traverse(assign.expr), assign.tp)(
      assign.pos)
  }

  def traverse(parens: Parens): Parens = {
    Parens(traverse(parens.expr), parens.tp)(parens.pos)
  }

  def traverse(newArray: NewArray): NewArray = {
    NewArray(newArray.annotations.map(traverse),
      newArray.dimAnnotations.map(_.map(traverse)),
      newArray.dimensions.map(traverse), newArray.initializers.map(traverse),
      newArray.elemType.map(traverse), newArray.tp)(newArray.pos)
  }

  def traverse(methodInv: MethodInv): MethodInv = {
    MethodInv(traverse(methodInv.methodSel), methodInv.typeArgs.map(traverse),
      methodInv.args.map(traverse), methodInv.tp, methodInv.refDecl)(
      methodInv.pos)
  }

  def traverse(conditional: Conditional): Conditional = {
    Conditional(traverse(conditional.cond), traverse(conditional.trueExpr),
      traverse(conditional.falseExpr), conditional.tp)(conditional.pos)
  }

  def traverse(newClass: NewClass): NewClass = {
    NewClass(traverse(newClass.ident), newClass.typeArgs.map(traverse),
      newClass.args.map(traverse), newClass.classBody.map(traverse),
      newClass.enclExpr.map(traverse), newClass.tp)(newClass.pos)
  }

  def traverse(memberRef: MemberRef): MemberRef = {
    MemberRef(memberRef.name, memberRef.typeArgs.map(traverse),
      traverse(memberRef.qualExpr), memberRef.mode, memberRef.polyKind,
      memberRef.tp)(memberRef.pos)
  }

  def traverse(lambda: Lambda): Expr = {
    Lambda(lambda.params.map(traverse), traverse(lambda.body),
      lambda.bodyKind, lambda.tp)(lambda.pos)
  }

  def traverse(classDecl: ClassDecl): ClassDecl = {
    ClassDecl(classDecl.name, classDecl.symbol,
      classDecl.typeParams.map(traverse),
      classDecl.extendsCl.map(traverse),
      classDecl.implementsCl.map(traverse),
      classDecl.members.map(traverse))(classDecl.pos)
  }

  def traverse(varDecl: VarDecl): VarDecl = {
    VarDecl(varDecl.mods, varDecl.name, varDecl.nameExpr.map(traverse),
      varDecl.symbol, traverse(varDecl.varType),
      varDecl.init.map(traverse), varDecl.kind)(varDecl.pos)
  }

  def traverse(assert: Assert): Assert = {
    Assert(traverse(assert.cond), traverse(assert.detail))(assert.pos)
  }

  def traverse(thr: Throw): Throw = {
    Throw(traverse(thr.expr))(thr.pos)
  }

  def traverse(ret: Return): Return = {
    Return(ret.expr.map(traverse))(ret.pos)
  }

  def traverse(exprStmt: ExprStatement): ExprStatement = {
    ExprStatement(traverse(exprStmt.expr))(exprStmt.pos)
  }

  def traverse(ifStmt: If): If = {
    If(traverse(ifStmt.cond), traverse(ifStmt.thenStmt),
      ifStmt.elseStmt.map(traverse))(ifStmt.pos)
  }

  def traverse(block: Block): Block = {
    Block(block.statements.map(traverse), block.isStatic)(block.pos)
  }

  def traverse(tryStmt: TryStmt): TryStmt = {
    TryStmt(tryStmt.resources.map(traverse),
      traverse(tryStmt.body),
      tryStmt.catches.map(traverse),
      tryStmt.finallyBlk.map(traverse))(tryStmt.pos)
  }

  def traverse(caseStmt: Case): Case = {
    Case(traverse(caseStmt.pat), caseStmt.statements.map(traverse))(
      caseStmt.pos)
  }

  def traverse(switch: Switch): Switch = {
    Switch(traverse(switch.selector),
      switch.cases.map(traverse))(switch.pos)
  }

  def traverse(synchronized: Synchronized): Synchronized = {
    Synchronized(traverse(synchronized.lock),
      traverse(synchronized.body))(synchronized.pos)
  }

  def traverse(labeledStmt: LabeledStmt): LabeledStmt = {
    LabeledStmt(labeledStmt.label, traverse(labeledStmt.body))(
      labeledStmt.pos)
  }

  def traverse(enhForLoop: EnhancedForLoop): Statement = {
    EnhancedForLoop(traverse(enhForLoop.variable),
      traverse(enhForLoop.expr), traverse(enhForLoop.body))(enhForLoop.pos)
  }

  def traverse(forLoop: ForLoop): Statement = {
    ForLoop(forLoop.init.map(traverse), forLoop.cond.map(traverse),
      forLoop.update.map(traverse), traverse(forLoop.body))(forLoop.pos)
  }

  def traverse(whileLoop: WhileLoop): Statement = {
    WhileLoop(traverse(whileLoop.cond), traverse(whileLoop.body))(
      whileLoop.pos)
  }

  def traverse(doWhileLoop: DoWhileLoop): Statement = {
    DoWhileLoop(traverse(doWhileLoop.cond), traverse(doWhileLoop.body))(
      doWhileLoop.pos)
  }

  def traverse(tree: Tree): Tree = {
    tree match {
      case tree: CompilationUnit => traverse(tree)
      case tree: Import          => traverse(tree)
      case tree: MethodDecl      => traverse(tree)
      case tree: TypeParam       => traverse(tree)
      case tree: CatchTree       => traverse(tree)
      case tree: Expr            => traverse(tree)
      case tree: Statement       => traverse(tree)
      case other                 => other
    }
  }

  def traverse(expression: Expr): Expr = {
    expression match {
      case expr: LetExpr          => traverse(expr)
      case expr: Annotation       => traverse(expr)
      case expr: AnnotatedType    => traverse(expr)
      case expr: Wildcard         => traverse(expr)
      case expr: TypeIntersection => traverse(expr)
      case expr: TypeUnion        => traverse(expr)
      case expr: TypeApply        => traverse(expr)
      case expr: ArrayTypeTree    => traverse(expr)
      case expr: Literal          => traverse(expr)
      case expr: Ident            => traverse(expr)
      case expr: FieldAccess      => traverse(expr)
      case expr: ArrayAccess      => traverse(expr)
      case expr: InstanceOf       => traverse(expr)
      case expr: TypeCast         => traverse(expr)
      case expr: Binary           => traverse(expr)
      case expr: Unary            => traverse(expr)
      case expr: AssignOp         => traverse(expr)
      case expr: Assign           => traverse(expr)
      case expr: Parens           => traverse(expr)
      case expr: NewArray         => traverse(expr)
      case expr: MethodInv        => traverse(expr)
      case expr: Conditional      => traverse(expr)
      case expr: NewClass         => traverse(expr)
      case expr: MemberRef        => traverse(expr)
      case expr: Lambda           => traverse(expr)
      case other                  => other
    }
  }

  def traverse(stmt: Statement): Statement = {
    stmt match {
      case stmt: VarDecl         => traverse(stmt)
      case stmt: ClassDecl       => traverse(stmt)
      case stmt: Assert          => traverse(stmt)
      case stmt: Throw           => traverse(stmt)
      case stmt: Return          => traverse(stmt)
      case stmt: ExprStatement   => traverse(stmt)
      case stmt: If              => traverse(stmt)
      case stmt: Block           => traverse(stmt)
      case stmt: TryStmt         => traverse(stmt)
      case stmt: Case            => traverse(stmt)
      case stmt: Switch          => traverse(stmt)
      case stmt: Synchronized    => traverse(stmt)
      case stmt: LabeledStmt     => traverse(stmt)
      case stmt: EnhancedForLoop => traverse(stmt)
      case stmt: ForLoop         => traverse(stmt)
      case stmt: WhileLoop       => traverse(stmt)
      case stmt: DoWhileLoop     => traverse(stmt)
      case other                 => other
    }
  }
}
