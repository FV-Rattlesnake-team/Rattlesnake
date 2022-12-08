package compiler.verification

import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep}
import lang.{Operator, Types}
import lang.Operator.*
import lang.Types.PrimitiveType.{DoubleType, IntType, StringType}
import smtlib.theories.Core
import smtlib.theories.Core.{Equals, Implies, Not}
import smtlib.theories.Ints.{Add as IAdd, Div as IDiv, LessThan as ILessThan, Mod as IMod, Mul as IMul, Sub as ISub}
import smtlib.theories.Operations.OperationN1
import smtlib.theories.Reals.{Add as RAdd, Div as RDiv, LessThan as RLessThan, Mul as RMul, Sub as RSub}
import smtlib.trees.Commands.Assert
import smtlib.trees.Terms.*

import java.io.FileWriter
import scala.collection.mutable.ListBuffer

final class FormulasWriter(outputFilePath: String) extends CompilerStep[(List[Source], AnalysisContext), Unit] {

  private final case class Path(precond: List[Term], postcond: Term)

  private final case class Ctx(
                                knownFormulas: ListBuffer[Term],
                                paths: ListBuffer[Path],
                                analysisContext: AnalysisContext,
                                varsCtx: LocalVarsCtx
                              ){
    def copyForNewBlock: Ctx = {
      Ctx(
        ListBuffer.from(knownFormulas),
        paths,
        analysisContext,
        varsCtx.copied
      )
    }
  }

  private val numericOperators = Map[Operator, (OperationN1, OperationN1)](
    Plus -> (IAdd, RAdd),
    Minus -> (ISub, RSub),
    Times -> (IMul, RMul),
    Div -> (IDiv, RDiv)
  )

  override def apply(input: (List[Source], AnalysisContext)): Unit = {
    val variablesRenamingCtx = LocalVarsCtx.newInstance()

    val writer = new FileWriter(outputFilePath)

    ???
  }

  extension (l: Term) {
    private def and(r: Term): Term = Core.And(l, r)
    private def or(r: Term): Term = Core.Or(l, r)
  }

  private def addFormulasNoRet(stat: Statement, ctx: Ctx): Unit = {
    stat match

      case expr: Expr =>
        addFormulasRetExpr(expr, ctx)

      case Block(stats) =>
        val newCtx = ctx.copyForNewBlock
        for stat <- stats do {
          addFormulasNoRet(stat, newCtx)
        }

      case LocalDef(rawName, optType, rhs, _) =>
        val tpe = optType.getOrElse(rhs.getType)
        addAssigFormula(rawName, rhs, tpe, ctx)

      case VarAssig(VariableRef(rawName), rhs) =>
        val tpe = rhs.getType
        addAssigFormula(rawName, rhs, tpe, ctx)

      case VarAssig(Indexing(indexed, arg), rhs) =>
        // TODO arrays
        ???

      case VarAssig(Select(lhs, selected), rhs) =>
        // TODO structs
        ???

      case IfThenElse(cond, thenBr, elseBrOpt) => ???
      case WhileLoop(cond, body, _) => ???
      case ReturnStat(optVal) => ???
      case PanicStat(msg) => ???
      case Assertion(formulaExpr, descr, isAssumed) => ???

      case _: (VarModif | ForLoop) => assert(false)
  }

  private def addFormulasRetExpr(expr: Expr, ctx: Ctx): Term = {
    val Ctx(knownFormulas, paths, analysisCtx, varsCtx) = ctx
    expr match
      case IntLit(value) =>
        SNumeral(value)
      case DoubleLit(value) =>
        SDecimal(value)
      case CharLit(value) =>
        SNumeral(value.toInt)
      case BoolLit(value) =>
        SBinary(List(value))
      case StringLit(value) =>
        SString(value)
      case VariableRef(rawName) =>
        symbol(varsCtx.currNameFor(rawName).get)

      // TODO arrays ------------------------
      case Indexing(indexed, arg) =>
        // TODO check on array size
        ???
      case ArrayInit(elemType, size) =>
        ???
      case FilledArrayInit(arrayElems) =>
        ???
      // ------------------------------------

      // TODO structs -----------------------
      case StructInit(structName, args) =>
        ???
      case Select(lhs, selected) =>
        ???
      // ------------------------------------

      case UnaryOp(operator, operand) =>
        ??? // TODO (only sharp operator)

      case BinaryOp(lhs, Equality, rhs) =>
        Equals(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx))

      case BinaryOp(lhs, LessThan, rhs) if lhs.getType.subtypeOf(IntType) =>
        ILessThan(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx))

      case BinaryOp(lhs, LessThan, rhs) if lhs.getType.subtypeOf(DoubleType) =>
        RLessThan(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx))

      case BinaryOp(lhs, Plus, rhs) if lhs.getType.subtypeOf(StringType) =>
        ??? // TODO + on strings

      case BinaryOp(lhs, Modulo, rhs) =>
        IMod(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx))

      case BinaryOp(lhs, op, rhs) => {
        assert(numericOperators.contains(op))
        val (iop, rop) = numericOperators.apply(op)
        if (lhs.getType.subtypeOf(IntType)) {
          iop(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx))
        } else if (lhs.getType.subtypeOf(DoubleType)) {
          rop(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx))
        } else {
          throw new AssertionError()
        }
      }

      case Ternary(cond, thenBr, elseBr) =>
        // FIXME
        ???

      case Cast(expr, tpe) =>
        ??? // TODO cast

      case Sequence(stats, expr) =>
        ??? // FIXME

      case Call(callee, args) => ??? // FIXME
  }

  private def addAssigFormula(rawName: String, rhs: Expr, tpe: Types.Type, ctx: Ctx): Unit = {
    val name = ctx.varsCtx.addAndRetNameFor(rawName, tpe)
    val assigFormula = Equals(symbol(name), addFormulasRetExpr(rhs, ctx))
    ctx.knownFormulas.addOne(assigFormula)
  }

  private def symbol(str: String): QualifiedIdentifier = {
    QualifiedIdentifier(SimpleIdentifier(SSymbol(str)))
  }


}
