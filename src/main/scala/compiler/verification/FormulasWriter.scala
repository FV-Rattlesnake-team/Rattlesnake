package compiler.verification

import compiler.FunctionalChecker.isPurelyFunctional
import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.prettyprinter.PrettyPrinter
import compiler.{AnalysisContext, CompilerStep, FunctionalChecker}
import lang.Operator.*
import lang.Types.PrimitiveType.*
import lang.{Operator, Types}
import smtlib.theories.Core
import smtlib.theories.Core.{Equals, Implies, Not}
import smtlib.theories.Ints.{GreaterEquals, Add as IAdd, Div as IDiv, LessThan as ILessThan, Mod as IMod, Mul as IMul, Sub as ISub}
import smtlib.theories.Operations.OperationN1
import smtlib.theories.Reals.{Add as RAdd, Div as RDiv, LessThan as RLessThan, Mul as RMul, Sub as RSub}
import smtlib.trees.Commands.{Assert, Script, FunDef as SmtFunDef}
import smtlib.trees.Terms.*

import java.io.FileWriter
import scala.collection.mutable.ListBuffer

final class FormulasWriter() extends CompilerStep[(List[Source], AnalysisContext), Unit] {
  private val arraySizeFunName = "##sizeof"
  private val arrayIndexingFunName = "##arrayAccess"

  private final case class Path(precond: List[Term], postcond: Term, descr: String) {
    override def toString: String = {
      val precondlines = precond.map(_.toString)
      val nDashes = precondlines.maxBy(_.length).length
      val dashesLines = "\n" ++ "-".repeat(nDashes) ++ "\n"
      precondlines.mkString("\n") ++ dashesLines ++ postcond.toString
    }
  }

  private final case class Ctx(
                                knownFormulas: ListBuffer[Term],
                                paths: ListBuffer[Path],
                                analysisContext: AnalysisContext,
                                varsCtx: LocalVarsCtx
                              ) {
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
        case _: StructDef => () // ignore
        case FunDef(_, params, _, body, _, _) => {
          val funCtx = ctx.copyForNewBlock
          for Param(paramName, tpe) <- params do {
            funCtx.varsCtx.addAndRetNameFor(paramName, tpe)
          }
          addFormulas(body, funCtx)
        }
      }
    }

    // FIXME
    for path <- paths do {
      println(path)
      println
    }
  }

  extension (l: Term) {
    private def and(r: Term): Term = Core.And(l, r)
    private def or(r: Term): Term = Core.Or(l, r)
  }

  private def addFormulas(stat: Statement, ctx: Ctx): Option[Term] = {
    val Ctx(knownFormulas, paths, analysisContext, varsCtx) = ctx

    def addFormulasRetExpr(expr: Asts.Expr, context: Ctx): Term = addFormulas(expr, context).get
    extension(term: Term) def toSome: Option[Term] = Some(term)

    stat match {

      case IntLit(value) =>
        SNumeral(value).toSome
      case DoubleLit(value) =>
        SDecimal(value).toSome
      case CharLit(value) =>
        SNumeral(value.toInt).toSome
      case BoolLit(value) =>
        SBinary(List(value)).toSome
      case StringLit(value) =>
        SString(value).toSome
      case VariableRef(rawName) =>
        symbol(varsCtx.currNameFor(rawName)).toSome

      // TODO arrays ------------------------
      case Indexing(_, arg) =>
        // TODO check on array index vs size
        FunctionApplication(symbol(arrayIndexingFunName), List(addFormulasRetExpr(arg, ctx))).toSome
      case ArrayInit(elemType, size) =>
        ???
        // TODO predicate on size
      case FilledArrayInit(arrayElems) =>
        ???
        // TODO predicate on size
      // ------------------------------------

      // TODO structs -----------------------
      case StructInit(structName, args) =>
        ???
      case Select(lhs, selected) =>
        ???
      // ------------------------------------

      case UnaryOp(ExclamationMark, operand) =>
        Not(addFormulasRetExpr(operand, ctx)).toSome

      case UnaryOp(Sharp, operand) =>
        FunctionApplication(symbol(arraySizeFunName), List(addFormulasRetExpr(operand, ctx))).toSome

      case BinaryOp(lhs, Equality, rhs) =>
        Equals(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx)).toSome

      case BinaryOp(lhs, LessThan, rhs) if lhs.getType.subtypeOf(IntType) =>
        ILessThan(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx)).toSome

      case BinaryOp(lhs, LessThan, rhs) if lhs.getType.subtypeOf(DoubleType) =>
        RLessThan(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx)).toSome

      case BinaryOp(lhs, Plus, rhs) if lhs.getType.subtypeOf(StringType) =>
        ??? // TODO + on strings

      case BinaryOp(lhs, Modulo, rhs) =>
        IMod(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx)).toSome

      case BinaryOp(lhs, op, rhs) => {
        assert(numericOperators.contains(op))
        val (iop, rop) = numericOperators.apply(op)
        if (lhs.getType.subtypeOf(IntType)) {
          iop(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx)).toSome
        } else if (lhs.getType.subtypeOf(DoubleType)) {
          rop(addFormulasRetExpr(lhs, ctx), addFormulasRetExpr(rhs, ctx)).toSome
        } else {
          throw new AssertionError()
        }
      }

      case ternary@Ternary(cond, thenBr, elseBr) => {
        val condExpr = addFormulasRetExpr(cond, ctx)
        val thenExpr = addFormulasRetExpr(thenBr, ctx)
        val elseExpr = addFormulasRetExpr(elseBr, ctx)
        val symb = symbol(varsCtx.addAndRetUnnamedSymbol(ternary.getType))
        val formula = (Equals(symb, thenExpr) and condExpr) or (Equals(symb, elseExpr) and Not(condExpr))
        ctx.knownFormulas.addOne(formula)
        symb.toSome
      }

      case Cast(expr, tpe) =>
        ??? // TODO cast

      case Sequence(stats, expr) => {
        for stat <- stats do {
          addFormulas(stat, ctx)
        }
        addFormulasRetExpr(expr, ctx).toSome
      }

      case call@Call(VariableRef(name), args) =>
        None // FIXME Some if is predicate, None o.w.

      case Block(stats) =>
        val newCtx = ctx.copyForNewBlock
        for stat <- stats do {
          addFormulas(stat, newCtx)
        }
        None

      case localDef@LocalDef(rawName, optType, rhs, _) =>
        val tpe = optType.getOrElse(rhs.getType)
        addAssigFormula(rawName, rhs, tpe, ctx)
        None

      case VarAssig(VariableRef(rawName), rhs) =>
        val tpe = rhs.getType
        addAssigFormula(rawName, rhs, tpe, ctx)
        None

      case VarAssig(Indexing(indexed, arg), rhs) =>
        // TODO arrays
        ???

      case VarAssig(Select(lhs, selected), rhs) =>
        // TODO structs
        ???

      case IfThenElse(_, thenBr, elseBrOpt) => {
        addFormulas(thenBr, ctx)
        elseBrOpt.foreach(addFormulas(_, ctx))
        None
      }

      case WhileLoop(_, body, _) =>
        addFormulas(body, ctx)

      case Assertion(formulaExpr, descr, isAssumed) => {
        val termFormula = addFormulasRetExpr(formulaExpr, ctx)
        if (!isAssumed) {
          val path = Path(ctx.knownFormulas.toList, termFormula, descr)
          ctx.paths.addOne(path)
        }
        ctx.knownFormulas.addOne(termFormula)
        None
      }

      case PanicStat(msg) =>
        addFormulas(msg, ctx)
        addFormulas(Assertion(BoolLit(false).setType(BoolType), "panic"), ctx)
        None

      case ReturnStat(optVal) =>
        optVal.foreach(addFormulas(_, ctx))
        None

      case _: (VarModif | ForLoop) => assert(false)
    }
  }

  private def addAssigFormula(rawName: String, rhs: Expr, tpe: Types.Type, ctx: Ctx): Unit = {
    // must add the formula even if None (o.w. later it will not be found)
    val name = ctx.varsCtx.addAndRetNameFor(rawName, tpe)
    addFormulas(rhs, ctx) match {
      case Some(term) =>
        val assigFormula = Equals(symbol(name), term)
        ctx.knownFormulas.addOne(assigFormula)
      case None => ()
    }
  }

  private def symbol(str: String): QualifiedIdentifier = {
    QualifiedIdentifier(SimpleIdentifier(SSymbol(str)))
  }


}
