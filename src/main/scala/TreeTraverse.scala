import com.sun.source.tree.LambdaExpressionTree.BodyKind
import com.sun.source.tree.MemberReferenceTree
import com.sun.tools.javac.tree.JCTree

import scala.collection.JavaConversions._
import trees._


/**
  * Converts the JCTree into Scala representation (from `Tree.scala`).
  */
object TreeTraverse {

  /**
    * Generic traverse, for top-level nodes.
    */
  def traverse(tree: JCTree): Tree = tree match {
    case that: JCTree.JCCompilationUnit =>
      traverseCompilationUnit(that)

    case that: JCTree.JCMethodDecl =>
      traverseMethodDecl(that)

    case that: JCTree.JCCatch =>
      traverseCatch(that)

    case that: JCTree.JCImport =>
      traverseImport(that)

    case that: JCTree.JCExpression =>
      traverseExpr(that)

    case that: JCTree.JCStatement =>
      traverseStmt(that)

    case that: JCTree.JCTypeParameter =>
      traverseTypeParam(that)

    case that: JCTree.JCModifiers =>
      traverseModifiers(that)

    case that =>
      println("Node not handled yet: " + that.getTag.toString)
      Skip
  }

  /**
    * Expressions.
    */
  private def traverseExpr(expr: JCTree.JCExpression): Expr = expr match {
    case that: JCTree.LetExpr =>
      traverseLetExpr(that)

    case that: JCTree.JCErroneous =>
      traverseErroneous(that)

    case that: JCTree.JCAnnotatedType =>
      traverseAnnotatedType(that)

    case that: JCTree.JCAnnotation =>
      traverseAnnotation(that)

    case that: JCTree.JCWildcard =>
      traverseWildcard(that)

    case that: JCTree.JCTypeIntersection =>
      traverseTypeInter(that)

    case that: JCTree.JCTypeUnion =>
      traverseTypeUnion(that)

    case that: JCTree.JCTypeApply =>
      traverseTypeApply(that)

    case that: JCTree.JCArrayTypeTree =>
      traverseArrayTypeTree(that)

    case that: JCTree.JCPrimitiveTypeTree =>
      traversePrimitiveTypeTree(that)

    case that: JCTree.JCLiteral =>
      traverseLiteral(that)

    case that: JCTree.JCIdent =>
      traverseIdent(that)

    case that: JCTree.JCFieldAccess =>
      traverseFieldAccess(that)

    case that: JCTree.JCArrayAccess =>
      traverseArrayAccess(that)

    case that: JCTree.JCInstanceOf =>
      traverseInstanceOf(that)

    case that: JCTree.JCTypeCast =>
      traverseTypeCast(that)

    case that: JCTree.JCBinary =>
      traverseBinary(that)

    case that: JCTree.JCUnary =>
      traverseUnary(that)

    case that: JCTree.JCAssignOp =>
      traverseAssignOp(that)

    case that: JCTree.JCAssign =>
      traverseAssign(that)

    case that: JCTree.JCParens =>
      traverseParens(that)

    case that: JCTree.JCNewArray =>
      traverseNewArray(that)

    case that: JCTree.JCPolyExpression =>
      traversePolyExpr(that)  // TODO

  }

  /**
    * Functional expressions.
    */
  def traverseFunctionalExpr(funExpr: JCTree.JCFunctionalExpression): FuncExpr =
    if (funExpr == null) null else funExpr match {
      case that: JCTree.JCMemberReference =>
        traverseMemberRef(that)

      case that: JCTree.JCLambda =>
        traverseLambda(that)
  }

  /**
    * Statements.
    */
  private def traverseStmt(stmt: JCTree.JCStatement): Statement = stmt match {
    case that: JCTree.JCAssert =>
      traverseAssert(that)

    case that: JCTree.JCThrow =>
      traverseThrow(that)

    case that: JCTree.JCReturn =>
      traverseReturn(that)

    case that: JCTree.JCContinue =>
      traverseContinue(that)

    case that: JCTree.JCBreak =>
      traverseBreak(that)

    case that: JCTree.JCExpressionStatement =>
      traverseExprStmt(that)

    case that: JCTree.JCIf =>
      traverseIf(that)

    case that: JCTree.JCTry =>
      traverseTry(that)

    case that: JCTree.JCSynchronized =>
      traverseSynchronized(that)

    case that: JCTree.JCCase =>
      traverseCase(that)

    case that: JCTree.JCSwitch =>
      traverseSwitch(that)

    case that: JCTree.JCLabeledStatement =>
      traverseLabeledStmt(that)

    case that: JCTree.JCEnhancedForLoop =>
      traverseEnhancedForLoop(that)

    case that: JCTree.JCForLoop =>
      traverseForLoop(that)

    case that: JCTree.JCWhileLoop =>
      traverseWhileLoop(that)

    case that: JCTree.JCDoWhileLoop =>
      traverseDoWhileLoop(that)


    case that: JCTree.JCBlock =>
      traverseBlock(that)

    case that: JCTree.JCSkip =>
      traverseSkip(that)

    case that: JCTree.JCVariableDecl =>
      traverseVarDecl(that)

    case that: JCTree.JCClassDecl =>
      traverseClassDecl(that)
  }

  /**
    * Poly expressions.
    */
  private def traversePolyExpr(polyExpr: JCTree.JCPolyExpression): PolyExpr = polyExpr match {
    case that: JCTree.JCMethodInvocation =>
      traverseMethodInv(that)

    case that: JCTree.JCFunctionalExpression =>
      traverseFunctionalExpr(that)

    case that: JCTree.JCConditional =>
      traverseConditional(that)

    case that: JCTree.JCNewClass =>
      traverseNewClass(that)
  }

  def traverseConditional(conditional: JCTree.JCConditional): Conditional = {
    val cond = traverseExpr(conditional.getCondition)
    val trueE = traverseExpr(conditional.getTrueExpression)
    val falseE = traverseExpr(conditional.getFalseExpression)
    val tp = JExprType(conditional.`type`)

    Conditional(cond, trueE, falseE, tp)
  }

  def traverseNewClass(newClass: JCTree.JCNewClass): NewClass = {
    val typeArgs = newClass.getTypeArguments.map(traverseExpr).toList
    val args = newClass.getArguments.map(traverseExpr).toList
    val classBody = Option(newClass.getClassBody).map(traverseClassDecl)
    val enclExpr = Option(newClass.getEnclosingExpression).map(traverseExpr)
    val ident = traverseExpr(newClass.getIdentifier)
    val tp = JExprType(newClass.`type`)

    NewClass(ident, typeArgs, args, classBody, enclExpr, tp)
  }

  def traverseMethodInv(methodInv: JCTree.JCMethodInvocation): MethodInv = {
    val args = methodInv.getArguments.map(traverseExpr).toList
    val methodSel = traverseExpr(methodInv.getMethodSelect)
    val typeArgs = methodInv.getTypeArguments.map(traverseExpr).toList
    val tp = JExprType(methodInv.`type`)

    MethodInv(methodSel, typeArgs, args, tp)
  }

  /**
    * Specific per-node-type traversals.
    */
  private def traverseCompilationUnit(compUnit: JCTree.JCCompilationUnit): CompilationUnit = {
    val imports = compUnit.getImports.map(traverseImport).toList
    val typeDecls = compUnit.getTypeDecls.map(traverse).toList

    CompilationUnit(imports, typeDecls)
  }

  private def traverseMethodDecl(methodDecl: JCTree.JCMethodDecl): MethodDecl = {
    val name = Name.fromJName(methodDecl.getName)
    val symbol = methodDecl.sym
    val modifiers = traverseModifiers(methodDecl.getModifiers)
    val typeParams = methodDecl.getTypeParameters.map(traverseTypeParam).toList
    val recvParam = Option(methodDecl.getReceiverParameter).map(traverseVarDecl)
    val params = methodDecl.getParameters.map(traverseVarDecl).toList
    val thrown = methodDecl.getThrows.map(traverseExpr).toList
    val retType = Option(methodDecl.getReturnType).map(traverse)
    val body = traverseBlock(methodDecl.getBody)
    val defVal = Option(methodDecl.defaultValue).map(traverseExpr)

    MethodDecl(name, symbol, modifiers, typeParams, recvParam,
      params, thrown, retType, body, defVal)
  }

  private def traverseImport(that: JCTree.JCImport): Import = {
    val qId = traverse(that.getQualifiedIdentifier)
    Import(qId)
  }

  private def traverseTypeParam(tparam: JCTree.JCTypeParameter): TypeParam = {
    val name = Name.fromJName(tparam.getName)
    val bounds = tparam.getBounds.map(traverseExpr).toList
    val annotations = tparam.getAnnotations.map(traverseAnnotation).toList
    TypeParam(name, bounds, annotations)
  }

  private def traverseAnnotation(annot: JCTree.JCAnnotation): Annotation = {
    val annotType = traverse(annot.getAnnotationType)
    val args = annot.getArguments.map(traverseExpr).toList
    val tp = JExprType(annot.`type`)

    Annotation(annotType, args, tp)
  }

  private def traverseModifiers(modifiers: JCTree.JCModifiers): Modifiers = {
    val flags = modifiers.getFlags.toSet
    val annotations = modifiers.getAnnotations.map(traverseAnnotation).toList

    Modifiers(flags, annotations)
  }

  private def traverseVarDecl(varDecl: JCTree.JCVariableDecl): VarDecl = {
    val initializer = Option(varDecl.getInitializer).map(traverseExpr)
    val modifiers = traverseModifiers(varDecl.getModifiers)
    val name = Name.fromJName(varDecl.getName)
    val nameExpr = Option(varDecl.getNameExpression).map(traverseExpr)
    val tpe = traverse(varDecl.getType)

    VarDecl(modifiers, name, nameExpr, tpe, initializer)
  }

  private def traverseClassDecl(classDecl: JCTree.JCClassDecl): ClassDecl = {
    val typeParams = classDecl.getTypeParameters.map(traverseTypeParam).toList
    val extendsCl = Option(classDecl.getExtendsClause).map(traverseExpr)
    val implementsCl = classDecl.getImplementsClause.map(traverseExpr).toList
    val members = classDecl.getMembers.map(traverse).toList

    ClassDecl(typeParams, extendsCl, implementsCl, members)
  }

  private def traverseLetExpr(letExpr: JCTree.LetExpr): LetExpr = {
    val defs = letExpr.defs.map(traverseVarDecl).toList
    val expr = traverse(letExpr.expr)
    val tp = JExprType(letExpr.`type`)

    LetExpr(defs, expr, tp)
  }

  private def traverseErroneous(erroneous: JCTree.JCErroneous): Erroneous = {
    val trees = erroneous.getErrorTrees.map(traverse).toList
    val tp = JExprType(erroneous.`type`)

    Erroneous(trees, tp)
  }

  private def traverseAnnotatedType(annType: JCTree.JCAnnotatedType): AnnotatedType = {
    val anns = annType.getAnnotations.map(traverseAnnotation).toList
    val underType = traverseExpr(annType.getUnderlyingType)
    val tp = JExprType(annType.`type`)

    AnnotatedType(anns, underType, tp)
  }

  private def traverseWildcard(wildcard: JCTree.JCWildcard): Wildcard = {
    val bound = traverse(wildcard.getBound)
    val tp = JExprType(wildcard.`type`)

    Wildcard(bound, tp)
  }

  private def traverseTypeInter(tInter: JCTree.JCTypeIntersection): TypeIntersection = {
    val bounds = tInter.getBounds.map(traverseExpr).toList
    val tp = JExprType(tInter.`type`)

    TypeIntersection(bounds, tp)
  }

  private def traverseTypeUnion(tUnion: JCTree.JCTypeUnion): TypeUnion = {
    val alternatives = tUnion.getTypeAlternatives.map(traverseExpr).toList
    val tp = JExprType(tUnion.`type`)

    TypeUnion(alternatives, tp)
  }

  private def traverseTypeApply(tApply: JCTree.JCTypeApply): TypeApply = {
    val tpe = traverse(tApply.getType)
    val tArgs = tApply.getTypeArguments.map(traverseExpr).toList
    val tp = JExprType(tApply.`type`)

    TypeApply(tpe, tArgs, tp)
  }

  private def traverseArrayTypeTree(arrTypeTree: JCTree.JCArrayTypeTree): ArrayTypeTree = {
    val elemType = traverse(arrTypeTree.getType)
    val tp = JExprType(arrTypeTree.`type`)

    ArrayTypeTree(elemType, tp)
  }

  private def traversePrimitiveTypeTree(primTypeTree: JCTree.JCPrimitiveTypeTree): PrimitiveTypeTree = {
    val typeKind = primTypeTree.getPrimitiveTypeKind
    val typeTag = primTypeTree.typetag
    val tp = JExprType(primTypeTree.`type`)

    PrimitiveTypeTree(typeKind, typeTag, tp)
  }

  private def traverseLiteral(literal: JCTree.JCLiteral): Literal = {
    val typeTag = literal.typetag
    val value = literal.getValue
    val tp = JExprType(literal.`type`)

    Literal(typeTag, value, tp)
  }

  private def traverseIdent(ident: JCTree.JCIdent): Ident = {
    val symbol = ident.sym
    val name = Name.fromJName(ident.getName)
    val tp = JExprType(ident.`type`)

    Ident(symbol, name, tp)
  }

  private def traverseFieldAccess(fieldAccess: JCTree.JCFieldAccess): FieldAccess = {
    val name = Name.fromJName(fieldAccess.getIdentifier)
    val selected = traverseExpr(fieldAccess.getExpression)
    val symbol = fieldAccess.sym
    val tp = JExprType(fieldAccess.`type`)

    FieldAccess(name, symbol, selected, tp)
  }

  private def traverseArrayAccess(arrayAccess: JCTree.JCArrayAccess): ArrayAccess = {
    val indexed = traverseExpr(arrayAccess.getExpression)
    val index = traverseExpr(arrayAccess.getIndex)
    val tp = JExprType(arrayAccess.`type`)

    ArrayAccess(indexed, index, tp)
  }

  private def traverseInstanceOf(instOf: JCTree.JCInstanceOf): InstanceOf = {
    val clazz = traverse(instOf.getType)
    val expr = traverseExpr(instOf.getExpression)
    val tp = JExprType(instOf.`type`)

    InstanceOf(clazz, expr, tp)
  }

  private def traverseTypeCast(typeCast: JCTree.JCTypeCast): TypeCast = {
    val clazz  = traverse(typeCast.getType)
    val expr = traverseExpr(typeCast.getExpression)
    val tp = JExprType(typeCast.`type`)

    TypeCast(clazz, expr, tp)
  }

  private def traverseBinary(binary: JCTree.JCBinary): Binary = {
    val op = binary.getOperator
    val left = traverseExpr(binary.getLeftOperand)
    val right = traverseExpr(binary.getRightOperand)
    val tp = JExprType(binary.`type`)

    Binary(op, left, right, tp)
  }

  private def traverseUnary(unary: JCTree.JCUnary): Unary = {
    val op = unary.getOperator
    val arg = traverseExpr(unary.getExpression)
    val tp = JExprType(unary.`type`)

    Unary(op, arg, tp)
  }

  private def traverseAssignOp(assignOp: JCTree.JCAssignOp): AssignOp = {
    val variable = traverseExpr(assignOp.getVariable)
    val op = assignOp.getOperator
    val expr = traverseExpr(assignOp.getExpression)
    val tp = JExprType(assignOp.`type`)

    AssignOp(variable, op, expr, tp)
  }

  private def traverseAssign(assign: JCTree.JCAssign): Assign = {
    val variable = traverseExpr(assign.getVariable)
    val expr = traverseExpr(assign.getExpression)
    val tp = JExprType(assign.`type`)

    Assign(variable, expr, tp)
  }

  private def traverseParens(parens: JCTree.JCParens): Parens = {
    val expr = traverseExpr(parens.getExpression)
    val tp = JExprType(parens.`type`)

    Parens(expr, tp)
  }

  private def traverseNewArray(newArray: JCTree.JCNewArray): NewArray = {
    val annotations = newArray.getAnnotations.map(traverseAnnotation).toList
    val dimAnnotations = newArray.getDimAnnotations.map(_.map(traverseAnnotation).toList).toList
    val dimensions = newArray.getDimensions.map(traverseExpr).toList
    // TODO val initializers = newArray.getInitializers.map(traverseExpr).toList
    val elType = Option(newArray.getType).map(traverseExpr)
    val tp = JExprType(newArray.`type`)

    NewArray(annotations, dimAnnotations, dimensions, null, elType, tp)
  }

  private def traverseAssert(assert: JCTree.JCAssert): Assert = {
    val cond = traverseExpr(assert.getCondition)
    val detail = traverseExpr(assert.getDetail)

    Assert(cond, detail)
  }

  private def traverseThrow(throwStmt: JCTree.JCThrow): Throw = {
    val expr = traverseExpr(throwStmt.getExpression)

    Throw(expr)
  }

  private def traverseReturn(retStmt: JCTree.JCReturn): Return = {
    val expr = traverseExpr(retStmt.getExpression)

    Return(expr)
  }

  private def traverseContinue(contStmt: JCTree.JCContinue): Continue = {
    val label = Option(contStmt.getLabel).map(Name.fromJName)

    Continue(label)
  }

  private def traverseBreak(breakStmt: JCTree.JCBreak): Break = {
    val label = Option(breakStmt.getLabel).map(Name.fromJName)

    Break(label)
  }

  private def traverseExprStmt(exprStmt: JCTree.JCExpressionStatement): ExprStatement = {
    val expr = traverseExpr(exprStmt.getExpression)

    ExprStatement(expr)
  }

  private def traverseIf(ifStmt: JCTree.JCIf): If = {
    val cond = traverseExpr(ifStmt.getCondition)
    val thenStmt = traverseStmt(ifStmt.getThenStatement)
    val elseStmt = Option(ifStmt.getElseStatement).map(traverseStmt)

    If(cond, thenStmt, elseStmt)
  }

  private def traverseTry(tryStmt: JCTree.JCTry): TryStmt = {
    val resources = tryStmt.getResources.map(traverse).toList
    val body = traverseBlock(tryStmt.getBlock)
    val catches = tryStmt.getCatches.map(traverseCatch).toList
    val finallyBlk = Option(tryStmt.getFinallyBlock).map(traverseBlock)

    TryStmt(resources, body, catches, finallyBlk)
  }

  private def traverseCatch(catchStmt: JCTree.JCCatch): CatchTree = {
    val param = traverseVarDecl(catchStmt.getParameter)
    val body = traverseBlock(catchStmt.getBlock)

    CatchTree(param, body)
  }

  private def traverseBlock(block: JCTree.JCBlock): Block = {
    val statements = block.getStatements.map(traverseStmt).toList

    Block(statements, block.isStatic)
  }

  private def traverseSkip(skip: JCTree.JCSkip) = Skip

  private def traverseCase(caseStmt: JCTree.JCCase): Case = {
    val pat = traverseExpr(caseStmt.getExpression)
    val statements = caseStmt.getStatements.map(traverseStmt).toList

    Case(pat, statements)
  }

  private def traverseSwitch(switchStmt: JCTree.JCSwitch): Switch = {
    val selector = traverseExpr(switchStmt.getExpression)
    val cases = switchStmt.getCases.map(traverseCase).toList

    Switch(selector, cases)
  }

  private def traverseSynchronized(synchr: JCTree.JCSynchronized): Synchronized = {
    val lock = traverseExpr(synchr.getExpression)
    val body = traverseBlock(synchr.getBlock)

    Synchronized(lock, body)
  }

  private def traverseLabeledStmt(labeledStmt: JCTree.JCLabeledStatement): LabeledStmt = {
    val label = Name.fromJName(labeledStmt.getLabel)
    val body = traverseStmt(labeledStmt.body)

    LabeledStmt(label, body)
  }

  private def traverseEnhancedForLoop(enhForLoop: JCTree.JCEnhancedForLoop): EnhancedForLoop = {
    val variable = traverseVarDecl(enhForLoop.getVariable)
    val expr = traverseExpr(enhForLoop.getExpression)
    val body = traverseStmt(enhForLoop.getStatement)

    EnhancedForLoop(variable, expr, body)
  }

  private def traverseForLoop(forLoop: JCTree.JCForLoop): ForLoop = {
    val init = forLoop.getInitializer.map(traverseStmt).toList
    val cond = Option(forLoop.getCondition).map(traverseExpr)
    val update = forLoop.getUpdate.map(traverseExprStmt).toList
    val body = traverseStmt(forLoop.getStatement)

    ForLoop(init, cond, update, body)
  }

  private def traverseWhileLoop(whileLoop: JCTree.JCWhileLoop): WhileLoop = {
    val cond = traverseExpr(whileLoop.getCondition)
    val body = traverseStmt(whileLoop.getStatement)

    WhileLoop(cond, body)
  }

  private def traverseDoWhileLoop(doWhileLoop: JCTree.JCDoWhileLoop): DoWhileLoop = {
    val cond = traverseExpr(doWhileLoop.getCondition)
    val body = traverseStmt(doWhileLoop.getStatement)

    DoWhileLoop(cond, body)
  }

  private def traverseMemberRef(memberRef: JCTree.JCMemberReference): MemberRef = {
    val name = Name.fromJName(memberRef.getName)
    val typeArgs = memberRef.getTypeArguments.map(traverseExpr).toList
    val qualExpr = traverseExpr(memberRef.getQualifierExpression)
    val mode = if (memberRef.getMode == MemberReferenceTree.ReferenceMode.INVOKE) Invoke else New
    val polyKind = if (memberRef.isPoly) Poly else Standalone
    val tp = JExprType(memberRef.`type`)

    MemberRef(name, typeArgs, qualExpr, mode, polyKind, tp)
  }

  private def traverseLambda(lambda: JCTree.JCLambda): Lambda = {
    val body = traverse(lambda.getBody)
    val bodyKind = if (lambda.getBodyKind == BodyKind.EXPRESSION) ExpressionKind else StatementKind
    val params = lambda.params.map(traverseVarDecl).toList
    val tp = JExprType(lambda.`type`)

    Lambda(params, body, bodyKind, tp)
  }
}
