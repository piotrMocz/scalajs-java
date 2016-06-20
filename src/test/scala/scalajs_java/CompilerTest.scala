package scalajs_java

import javax.lang.model.`type`.TypeKind

import com.sun.tools.javac.code.Symbol.VarSymbol
import com.sun.tools.javac.code.Type.JCPrimitiveType
import com.sun.tools.javac.code.TypeTag
import com.sun.tools.javac.tree.JCTree.Tag
import org.junit.Assert._
import org.junit.Test
import org.scalajs.core.ir
import org.scalajs.core.ir.Definitions._
import org.scalajs.core.ir.{Trees => irt, Types => irtpe}

import scalajs_java.compiler._
import scalajs_java.trees._

/** Whitebox tests */
class CompilerTest {

  private implicit val DummyPos = ir.Position(ir.Position.SourceFile(Config.testFilePath), 0, 0)
  private implicit val DummyPos2 = Position(0)

  private val MainObjectFullName = Compiler.MainObjectFullName
  private val MainClassFullName = MainObjectFullName + "$"

  // Could be useful in tests, depending on the trees you generate
  private val classType = irtpe.ClassType(encodeClassName(MainClassFullName))

  private def assertCompile(expected: irt.Tree, sourceTree: Tree): Unit = {

    def hashOf(body: irt.Tree): irt.TreeHash = {
      // Can only hash entire methods
      val methodDef = irt.MethodDef(static = false, irt.Ident("main__D"),
        Nil, irtpe.DoubleType, body)(
        irt.OptimizerHints.empty, None)
      ir.Hashers.hashMethodDef(methodDef).hash.get
    }

    val expectedHash = hashOf(expected)
    val actual = Compiler.compileTree(sourceTree)
    val actualHash = hashOf(actual)

    assertTrue(s"Expected $expected but got $actual",
      ir.Hashers.hashesEqual(actualHash, expectedHash, considerPos = true))
  }

  @Test def compileLiteral(): Unit = {
    assertCompile(irt.BooleanLiteral(true), TestUtils.booleanLiteral(true))
    assertCompile(irt.IntLiteral(234), TestUtils.charLiteral(234))
    assertCompile(irt.IntLiteral(234), TestUtils.intLiteral(234))
    assertCompile(irt.LongLiteral(234), TestUtils.longLiteral(234))
    assertCompile(irt.FloatLiteral(234), TestUtils.floatLiteral(234))
    assertCompile(irt.DoubleLiteral(234), TestUtils.doubleLiteral(234))
  }

  @Test def compileBinaryAdd(): Unit = {
    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Int_+, irt.IntLiteral(1), irt.IntLiteral(2)),
      Binary(Tag.PLUS, TestUtils.charLiteral(1), TestUtils.charLiteral(2), JExprType.charType()))

    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Int_+, irt.IntLiteral(1), irt.IntLiteral(2)),
      Binary(Tag.PLUS, TestUtils.intLiteral(1), TestUtils.intLiteral(2), JExprType.intType()))

    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Long_+, irt.LongLiteral(1), irt.LongLiteral(2)),
      Binary(Tag.PLUS, TestUtils.longLiteral(1), TestUtils.longLiteral(2), JExprType.longType()))

    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Float_+, irt.FloatLiteral(1), irt.FloatLiteral(2)),
      Binary(Tag.PLUS, TestUtils.floatLiteral(1), TestUtils.floatLiteral(2), JExprType.floatType()))

    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Double_+, irt.DoubleLiteral(1), irt.DoubleLiteral(2)),
      Binary(Tag.PLUS, TestUtils.doubleLiteral(1), TestUtils.doubleLiteral(2), JExprType.doubleType()))
  }

  @Test def compileBinarySub(): Unit = {
    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Int_-, irt.IntLiteral(1), irt.IntLiteral(2)),
      Binary(Tag.MINUS, TestUtils.charLiteral(1), TestUtils.charLiteral(2), JExprType.charType()))

    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Int_-, irt.IntLiteral(1), irt.IntLiteral(2)),
      Binary(Tag.MINUS, TestUtils.intLiteral(1), TestUtils.intLiteral(2), JExprType.intType()))

    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Long_-, irt.LongLiteral(1), irt.LongLiteral(2)),
      Binary(Tag.MINUS, TestUtils.longLiteral(1), TestUtils.longLiteral(2), JExprType.longType()))

    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Float_-, irt.FloatLiteral(1), irt.FloatLiteral(2)),
      Binary(Tag.MINUS, TestUtils.floatLiteral(1), TestUtils.floatLiteral(2), JExprType.floatType()))

    assertCompile(
      irt.BinaryOp(irt.BinaryOp.Double_-, irt.DoubleLiteral(1), irt.DoubleLiteral(2)),
      Binary(Tag.MINUS, TestUtils.doubleLiteral(1), TestUtils.doubleLiteral(2), JExprType.doubleType()))
  }

  @Test def compilePostIncrement(): Unit = {
    val irVarRef = irt.VarRef(irt.Ident("x"))(irtpe.IntType)
    val irTmpVarDef = irt.VarDef(irt.Ident("tmp12345"), irtpe.IntType,
      mutable = true, irVarRef)
    val irTmpVarRef = irt.VarRef(irt.Ident("tmp12345"))(irtpe.IntType)
    val irAssign = irt.Assign(irVarRef, irt.BinaryOp(irt.BinaryOp.Int_+, irVarRef, irt.IntLiteral(1)))
    val irBlock = irt.Block(irTmpVarDef, irAssign, irTmpVarRef)

    val intTypeTree = new PrimitiveTypeTree(TypeKind.INT, TypeTag.INT,
      JExprType.intType())
    val varType = new JCPrimitiveType(TypeTag.INT, null)
    val varSym = new VarSymbol(0, TestUtils.makeName("x"),
      varType, null) {
      override def isLocal = true
    }
    val varDef = VarDecl(Modifiers(Set.empty, Nil), Name("x"), None,
      varSym, intTypeTree, Some(TestUtils.intLiteral(42)), LocalVar)
    val varRef = Ident(varSym, Name("x"), JExprType.intType(), Some((varDef, LocalVar)))

    assertCompile(irBlock, Unary(Tag.POSTINC, varRef, JExprType.intType()))
  }

  @Test def compilePreIncrement(): Unit = {
    val irVarRef = irt.VarRef(irt.Ident("x$2", Some("x")))(irtpe.IntType) // TODO why x$2? :)
    val irAssign = irt.Assign(irVarRef, irt.BinaryOp(irt.BinaryOp.Int_+, irVarRef, irt.IntLiteral(1)))
    val irBlock = irt.Block(irAssign, irVarRef)

    val intTypeTree = new PrimitiveTypeTree(TypeKind.INT, TypeTag.INT,
      JExprType.intType())
    val varType = new JCPrimitiveType(TypeTag.INT, null)
    val varSym = new VarSymbol(0, TestUtils.makeName("x"),
      varType, null) {
      override def isLocal = true
    }
    val varDef = VarDecl(Modifiers(Set.empty, Nil), Name("x"), None,
      varSym, intTypeTree, Some(TestUtils.intLiteral(42)), LocalVar)
    val varRef = Ident(varSym, Name("x"), JExprType.intType(), Some((varDef, LocalVar)))

    assertCompile(irBlock, Unary(Tag.PREINC, varRef, JExprType.intType()))
  }

}
