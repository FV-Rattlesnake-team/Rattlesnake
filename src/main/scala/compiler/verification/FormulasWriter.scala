package compiler.verification

import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep}
import lang.{Operator, Types}
import lang.Operator.*
import lang.Types.PrimitiveType.{BoolType, DoubleType, IntType, StringType}
import smtlib.theories.Core
import smtlib.theories.Core.{Equals, Implies, Not}
import smtlib.theories.Ints.{Add as IAdd, Div as IDiv, LessThan as ILessThan, Mod as IMod, Mul as IMul, Sub as ISub}
import smtlib.theories.Operations.OperationN1
import smtlib.theories.Reals.{Add as RAdd, Div as RDiv, LessThan as RLessThan, Mul as RMul, Sub as RSub}
import smtlib.trees.Commands.{Assert, Script}
import smtlib.trees.Terms.*

import java.io.FileWriter
import scala.collection.mutable.ListBuffer

final class FormulasWriter() extends CompilerStep[(List[Source], AnalysisContext), Unit] {

  private final case class Path(precond: List[Term], postcond: Term, descr: String){
    override def toString: String = s"$precond  |-  $postcond  [$descr]"
  }

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
    val (sources, analysisContext) = input
    val paths = ListBuffer.empty[Path]
    val ctx = Ctx(ListBuffer.empty, paths, analysisContext, LocalVarsCtx.newInstance())
//    val writer = new FileWriter(outputFilePath)
    for {
      src <- sources
      defn <- src.defs
    } do {
      defn match {
        case _: StructDef => ()   // ignore
        case funDef: FunDef => addFormulasNoRet(funDef.body, ctx)
      }
    }
    
    // FIXME
    for path <- paths do {
      println(path)
    }
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

      case IfThenElse(_, thenBr, elseBrOpt) => {
        addFormulasNoRet(thenBr, ctx)
        elseBrOpt.foreach(addFormulasNoRet(_, ctx))
      }

      case WhileLoop(_, body, _) =>
        addFormulasNoRet(body, ctx)

      case Assertion(formulaExpr, descr, isAssumed) => {
        val termFormula = addFormulasRetExpr(formulaExpr, ctx)
        if (!isAssumed){
          val path = Path(ctx.knownFormulas.toList, termFormula, descr)
          ctx.paths.addOne(path)
        }
        ctx.knownFormulas.addOne(termFormula)
      }

      case PanicStat(_) =>
        addFormulasNoRet(Assertion(BoolLit(false).setType(BoolType), "panic"), ctx)

      case ReturnStat(_) => ()
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
