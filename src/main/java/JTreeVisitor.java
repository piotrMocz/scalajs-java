import com.sun.source.tree.VariableTree;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.util.List;

import javax.lang.model.element.Modifier;
import java.util.LinkedList;


public class JTreeVisitor extends JCTree.Visitor {

    private LinkedList<JCTree> stack;
    private int indent;

    public JTreeVisitor() {
        super();
        this.stack = new LinkedList<JCTree>();
        this.indent = 0;
    }

    private void push(JCTree tree) {
        this.stack.push(tree);
    }

    private JCTree pop(JCTree tree) {
        return this.stack.pop();
    }

    private String mkIndent() {
        return new String(new char[this.indent]).replace("\0", "  ");
    }

    private void indent() {
        this.indent += 1;
    }

    private void unindent() {
        this.indent -= 1;
    }

    private void acceptOpt(JCTree tree) {
        if (tree != null) {
            indent();

            tree.accept(this);

            unindent();
        }
    }

    private void acceptOpt(List<? extends JCTree> tree) {
        indent();

        for (JCTree tr : tree)
            acceptOpt(tr);

        unindent();
    }

    private void printInd(String s) {
        System.out.println(mkIndent() + s);
    }

    @Override
    public void visitTopLevel(JCTree.JCCompilationUnit that) {
        // super.visitTopLevel(that);
        printInd("JCCompilationUnit");

        indent();

        printInd("imports:");
        acceptOpt(that.getImports());
//            imp.accept(this);

        printInd("type decls:");
        acceptOpt(that.getTypeDecls());
//        for (JCTree td : that.getTypeDecls())
//            td.accept(this);

        unindent();
    }

    @Override
    public void visitImport(JCTree.JCImport that) {
//        super.visitImport(that);
        printInd("JCImport");
        indent();

        that.qualid.accept(this);

        unindent();
    }

    @Override
    public void visitClassDef(JCTree.JCClassDecl that) {
//        super.visitClassDef(that);
        printInd("JCClassDecl");

        indent();

        printInd("simple name: " + that.getSimpleName());

        printInd("type params:");
        acceptOpt(that.getTypeParameters());

        printInd("extends:");
        acceptOpt(that.getExtendsClause());

        printInd("implements:");
        acceptOpt(that.getImplementsClause());

        printInd("members:");
        acceptOpt(that.getMembers());

        printInd("modifiers:");
        acceptOpt(that.getModifiers());

        unindent();
    }

    @Override
    public void visitMethodDef(JCTree.JCMethodDecl that) {
//        super.visitMethodDef(that);
        printInd("JCMethodDecl");

        printInd("Name: " + that.getName());

        indent();

        printInd("type params:");
        acceptOpt(that.getTypeParameters());

        printInd("body:");
        acceptOpt(that.getBody());

        printInd("default value:");
        acceptOpt(that.getDefaultValue());

        printInd("modifiers:");
        acceptOpt(that.getModifiers());

        printInd("parameters:");
        acceptOpt(that.getParameters());

        printInd("receiver parameter:");
        acceptOpt(that.getReceiverParameter());

        unindent();
    }

    @Override
    public void visitVarDef(JCTree.JCVariableDecl that) {
//        super.visitVarDef(that);
        printInd("JCVariableDecl");
        printInd("Name" + that.getName());

        indent();

        printInd("modifiers:");
        acceptOpt(that.getModifiers());

        printInd("name expression:");
        acceptOpt(that.getNameExpression());

        printInd("type:");
        acceptOpt(that.getType());

        printInd("initializer:");
        acceptOpt(that.getInitializer());

        unindent();
    }

    @Override
    public void visitSkip(JCTree.JCSkip that) {
//        super.visitSkip(that);
        printInd("JCSkip");
    }

    @Override
    public void visitBlock(JCTree.JCBlock that) {
//        super.visitBlock(that);
        printInd("JCBlock");

        indent();

        printInd("Static: " + that.isStatic());

        printInd("statements:");
        acceptOpt(that.getStatements());

        unindent();
    }

    @Override
    public void visitDoLoop(JCTree.JCDoWhileLoop that) {
//        super.visitDoLoop(that);
        printInd("JCDoWhileLoop");

        indent();

        printInd("condition:");
        acceptOpt(that.getCondition());

        printInd("statement:");
        acceptOpt(that.getStatement());

        unindent();
    }

    @Override
    public void visitWhileLoop(JCTree.JCWhileLoop that) {
//        super.visitWhileLoop(that);
        printInd("JCWhileLoop");

        indent();

        printInd("condition:");
        acceptOpt(that.getCondition());

        printInd("statement:");
        acceptOpt(that.getStatement());

        unindent();
    }

    @Override
    public void visitForLoop(JCTree.JCForLoop that) {
//        super.visitForLoop(that);
        printInd("JCForLoop");

        indent();

        printInd("initializer:");
        acceptOpt(that.getInitializer());
//        for (JCTree.JCStatement stm: that.getInitializer())
//            stm.accept(this);

        printInd("condition:");
        acceptOpt(that.getCondition());

        printInd("update:");
        acceptOpt(that.getUpdate());
//        for (JCTree.JCStatement stm : that.getUpdate())
//            stm.accept(this);

        printInd("statement:");
        acceptOpt(that.getStatement());

        unindent();
    }

    @Override
    public void visitForeachLoop(JCTree.JCEnhancedForLoop that) {
//        super.visitForeachLoop(that);
        printInd("JCEnhancedForLoop");

        indent();

        printInd("variable:");
        acceptOpt(that.getVariable());

        printInd("expression:");
        acceptOpt(that.getExpression());

        printInd("statement:");
        acceptOpt(that.getStatement());

        unindent();
    }

    @Override
    public void visitLabelled(JCTree.JCLabeledStatement that) {
//        super.visitLabelled(that);
        printInd("JCLabeledStatement");

        indent();

        printInd("label:" + that.getLabel());

        printInd("statement:");
        acceptOpt(that.getStatement());

        unindent();
    }

    @Override
    public void visitSwitch(JCTree.JCSwitch that) {
//        super.visitSwitch(that);
        printInd("JCSwitch");

        indent();

        printInd("expression:");
        acceptOpt(that.getExpression());

        printInd("cases:");
        acceptOpt(that.getCases());
//        for (JCTree.JCCase cas : that.getCases())
//            cas.accept(this);

        unindent();
    }

    @Override
    public void visitCase(JCTree.JCCase that) {
//        super.visitCase(that);
        printInd("JCCase");

        indent();

        printInd("expression:");
        acceptOpt(that.getExpression());

        printInd("statements:");
        acceptOpt(that.getStatements());
//        for (JCTree.JCStatement stm : that.getStatements())
//            stm.accept(this);

        unindent();
    }

    @Override
    public void visitSynchronized(JCTree.JCSynchronized that) {
//        super.visitSynchronized(that);
        printInd("JCSynchronized");

        indent();

        printInd("expression:");
        acceptOpt(that.getExpression());

        printInd("block:");
        acceptOpt(that.getBlock());

        unindent();
    }

    @Override
    public void visitTry(JCTree.JCTry that) {
//        super.visitTry(that);
        printInd("JCTry");

        indent();

        printInd("resources");
        for (JCTree jct : that.getResources())
            jct.accept(this);

        printInd("block:");
        acceptOpt(that.getBlock());

        printInd("catches:");
        acceptOpt(that.getCatches());

        printInd("finally:");
        acceptOpt(that.getFinallyBlock());

        unindent();
    }

    @Override
    public void visitCatch(JCTree.JCCatch that) {
//        super.visitCatch(that);
        printInd("JCCatch");

        indent();

        printInd("parameter:");
        acceptOpt(that.getParameter());

        printInd("block:");
        acceptOpt(that.getBlock());

        unindent();
    }

    @Override
    public void visitConditional(JCTree.JCConditional that) {
//        super.visitConditional(that);
        printInd("JCConditional");

        indent();

        printInd("condition:");
        acceptOpt(that.getCondition());

        printInd("true part:");
        acceptOpt(that.getTrueExpression());

        printInd("false part:");
        acceptOpt(that.getFalseExpression());

        unindent();
    }

    @Override
    public void visitIf(JCTree.JCIf that) {
//        super.visitIf(that);
        printInd("JCIf");

        indent();

        printInd("condition:");
        acceptOpt(that.getCondition());

        printInd("true part:");
        acceptOpt(that.getThenStatement());

        printInd("false part:");
        acceptOpt(that.getElseStatement());

        unindent();
    }

    @Override
    public void visitExec(JCTree.JCExpressionStatement that) {
//        super.visitExec(that);
        printInd("JCExprStmt");

        indent();

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitBreak(JCTree.JCBreak that) {
//        super.visitBreak(that);
        printInd("JCBreak");

        indent();

        printInd("label:");
        printInd((that.getLabel() != null) ? that.getLabel().toString() : "no label");

        printInd("target:");
        acceptOpt(that.target);

        unindent();
    }

    @Override
    public void visitContinue(JCTree.JCContinue that) {
//        super.visitContinue(that);
        printInd("JCContinue");

        indent();

        printInd("label:");
        printInd((that.getLabel() != null) ? that.getLabel().toString() : "no label");

        printInd("target:");
        acceptOpt(that.target);

        unindent();
    }

    @Override
    public void visitReturn(JCTree.JCReturn that) {
//        super.visitReturn(that);
        printInd("JCReturn");

        indent();

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitThrow(JCTree.JCThrow that) {
//        super.visitThrow(that);
        printInd("JCThrow");

        indent();

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitAssert(JCTree.JCAssert that) {
//        super.visitAssert(that);
        printInd("JCAssert");

        indent();

        printInd("condition:");
        acceptOpt(that.getCondition());

        printInd("detail:");
        acceptOpt(that.getDetail());

        unindent();
    }

    @Override
    public void visitApply(JCTree.JCMethodInvocation that) {
//        super.visitApply(that);
        printInd("JCMethodInv");

        indent();

        printInd("arguments:");
        acceptOpt(that.getArguments());

        printInd("method select:");
        acceptOpt(that.getMethodSelect());

        unindent();
    }

    @Override
    public void visitNewClass(JCTree.JCNewClass that) {
//        super.visitNewClass(that);
        printInd("JCNewClass");

        indent();

        printInd("arguments:");
        acceptOpt(that.getArguments());

        printInd("class body:");
        acceptOpt(that.getClassBody());

        printInd("enclosing expression:");
        acceptOpt(that.getEnclosingExpression());

        unindent();
    }

    @Override
    public void visitNewArray(JCTree.JCNewArray that) {
//        super.visitNewArray(that);
        printInd("JCNewArray");

        indent();

        printInd("annotations:");
        acceptOpt(that.getAnnotations());

        printInd("dim annotations:");
        for (List<JCTree.JCAnnotation> lst : that.getDimAnnotations())
            acceptOpt(lst);

        printInd("type:");
        acceptOpt(that.getType());

        unindent();
    }

    @Override
    public void visitLambda(JCTree.JCLambda that) {
//        super.visitLambda(that);
        printInd("JCLambda");

        indent();

        printInd("body:");
        acceptOpt(that.getBody());

        printInd("body kind: " + that.getBodyKind().toString());

        printInd("params:");
        for (VariableTree par : that.getParameters())
            ((JCTree) par).accept(this);

        unindent();
    }

    @Override
    public void visitParens(JCTree.JCParens that) {
//        super.visitParens(that);
        printInd("JCParens");

        indent();

        printInd("expresssion:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitAssign(JCTree.JCAssign that) {
//        super.visitAssign(that);
        printInd("JCAssign");

        indent();

        printInd("variable:");
        acceptOpt(that.getVariable());

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitAssignop(JCTree.JCAssignOp that) {
//        super.visitAssignop(that);
        printInd("JCAssignOp");

        indent();

        printInd("variable:");
        acceptOpt(that.getVariable());

        if (that.getOperator() != null)
            printInd("operator:" + that.getOperator().toString());

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitUnary(JCTree.JCUnary that) {
//        super.visitUnary(that);
        printInd("JCUnary");

        indent();

        if (that.getOperator() != null)
            printInd("operator:" + that.getOperator().toString());

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitBinary(JCTree.JCBinary that) {
//        super.visitBinary(that);
        printInd("JCBinary");

        indent();

        if (that.getOperator() != null)
            printInd("operation:" + that.getOperator().toString());

        printInd("left hand side:");
        acceptOpt(that.getLeftOperand());

        printInd("right hand side:");
        acceptOpt(that.getRightOperand());

        unindent();
    }

    @Override
    public void visitTypeCast(JCTree.JCTypeCast that) {
//        super.visitTypeCast(that);
        printInd("JCTypeCast");

        indent();

        printInd("type:");
        acceptOpt(that.getType());

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitTypeTest(JCTree.JCInstanceOf that) {
//        super.visitTypeTest(that);
        printInd("JCInstanceOf");

        indent();

        printInd("type:");
        acceptOpt(that.getType());

        printInd("type:");
        acceptOpt(that.getType());

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitIndexed(JCTree.JCArrayAccess that) {
//        super.visitIndexed(that);
        printInd("JCArrayAccess");

        indent();

        printInd("index:");
        acceptOpt(that.getIndex());

        printInd("indexed:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitSelect(JCTree.JCFieldAccess that) {
//        super.visitSelect(that);
        printInd("JCFieldAccess");

        indent();

        printInd("identifier: " + that.getIdentifier());

        printInd("expression:");
        acceptOpt(that.getExpression());

        unindent();
    }

    @Override
    public void visitReference(JCTree.JCMemberReference that) {
//        super.visitReference(that);
        printInd("JCMemberReference");

        indent();

        printInd("Name: " + that.getName());

        printInd("reference mode : " + that.getMode().toString());

        printInd("qualifier expression:");
        acceptOpt(that.getQualifierExpression());

        printInd("type args:");
        acceptOpt(that.getTypeArguments());

        unindent();
    }

    @Override
    public void visitIdent(JCTree.JCIdent that) {
//        super.visitIdent(that);
        printInd("JCIdent");

        indent();

        printInd("name: " + that.getName().toString());

        if (that.sym != null)
            printInd("symbol: " + that.sym.toString());

        unindent();
    }

    @Override
    public void visitLiteral(JCTree.JCLiteral that) {
//        super.visitLiteral(that);
        printInd("JCLiteral");

        indent();

        if (that.typetag != null)
            printInd("typetag: " + that.typetag.toString());

        unindent();
    }

    @Override
    public void visitTypeIdent(JCTree.JCPrimitiveTypeTree that) {
//        super.visitTypeIdent(that);
        printInd("JCPrimitiveTypeTree");

        indent();

        if (that.getPrimitiveTypeKind() != null)
            printInd("primitive type kind: " + that.getPrimitiveTypeKind());

        unindent();
    }

    @Override
    public void visitTypeArray(JCTree.JCArrayTypeTree that) {
//        super.visitTypeArray(that);
        printInd("JCArrayTypeTree");

        indent();

        printInd("type:");
        acceptOpt(that.getType());

        unindent();
    }

    @Override
    public void visitTypeApply(JCTree.JCTypeApply that) {
//        super.visitTypeApply(that);
        printInd("JCTypeApply");

        indent();

        printInd("type apply:");
        acceptOpt(that.getType());

        printInd("type args:");
        acceptOpt(that.getTypeArguments());

        unindent();
    }

    @Override
    public void visitTypeUnion(JCTree.JCTypeUnion that) {
//        super.visitTypeUnion(that);
        printInd("JCTypeUnion");

        indent();

        printInd("type alternatives:");
        acceptOpt(that.getTypeAlternatives());

        unindent();
    }

    @Override
    public void visitTypeIntersection(JCTree.JCTypeIntersection that) {
//        super.visitTypeIntersection(that);
        printInd("JCTypeIntersection");

        indent();

        printInd("type bounds:");
        acceptOpt(that.getBounds());

        unindent();
    }

    @Override
    public void visitTypeParameter(JCTree.JCTypeParameter that) {
//        super.visitTypeParameter(that);
        printInd("JCTypeParameter");

        indent();

        if (that.getName() != null)
            printInd("name: " + that.getName());

        printInd("type bounds:");
        acceptOpt(that.getBounds());

        printInd("type annotations:");
        acceptOpt(that.getAnnotations());

        unindent();
    }

    @Override
    public void visitWildcard(JCTree.JCWildcard that) {
//        super.visitWildcard(that);
        printInd("JCWildcard");

        indent();

        printInd("bound:");
        acceptOpt(that.getBound());

        unindent();
    }

    @Override
    public void visitTypeBoundKind(JCTree.TypeBoundKind that) {
//        super.visitTypeBoundKind(that);
        printInd("TypeBoundKind");

        indent();

        printInd("BoundKind: " + that.kind.toString());

        unindent();
    }

    @Override
    public void visitAnnotation(JCTree.JCAnnotation that) {
//        super.visitAnnotation(that);
        printInd("JCAnnotation");

        indent();

        printInd("annotation type:");
        acceptOpt(that.getAnnotationType());

        printInd("arguments:");
        acceptOpt(that.getArguments());

        unindent();
    }

    @Override
    public void visitModifiers(JCTree.JCModifiers that) {
//        super.visitModifiers(that);
        printInd("JCModifiers");

        indent();

        printInd("annotations:");
        acceptOpt(that.getAnnotations());

        printInd("modifiers:");
        indent();
        for (Modifier m : that.getFlags())
            printInd(m.toString());
        unindent();

        unindent();
    }

    @Override
    public void visitAnnotatedType(JCTree.JCAnnotatedType that) {
//        super.visitAnnotatedType(that);
        printInd("JCAnnotatedType");

        indent();

        printInd("annotations:");
        acceptOpt(that.getAnnotations());

        printInd("underlying type:");
        acceptOpt(that.getUnderlyingType());

        unindent();
    }

    @Override
    public void visitErroneous(JCTree.JCErroneous that) {
//        super.visitErroneous(that);
        printInd("JCErroneous");

        indent();

        printInd("error trees:");
        acceptOpt(that.getErrorTrees());

        unindent();
    }

    @Override
    public void visitLetExpr(JCTree.LetExpr that) {
//        super.visitLetExpr(that);
        printInd("LetExpr");

        indent();

        printInd("definitions:");
        acceptOpt(that.defs);

        printInd("expression:");
        acceptOpt(that.expr);

        unindent();
    }

    @Override
    public void visitTree(JCTree that) {
//        super.visitTree(that);
        printInd("JCTree");
    }
}
