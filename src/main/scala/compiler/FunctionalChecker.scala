package compiler

import compiler.irs.Asts
import Asts.*

object FunctionalChecker {

  def isPurelyFunctional(statement: Statement)(implicit analysisContext: AnalysisContext): Boolean = {
    statement match
      case _: Asts.Literal => true
      case VariableRef(_) => true
      case Call(name, args) => {
        def bodyIsFullyFunctional = {
          analysisContext.functions.apply(name).optDef match {
            case None => false // built-in function
            case Some(funDef) => isPurelyFunctional(funDef)
          }
        }
        args.forall(isPurelyFunctional) && bodyIsFullyFunctional
      }
      case Indexing(indexed, arg) =>
        isPurelyFunctional(indexed) && isPurelyFunctional(arg)
      case ArrayInit(_, size) =>
        isPurelyFunctional(size)
      case FilledArrayInit(arrayElems) =>
        arrayElems.forall(isPurelyFunctional)
      case StructInit(_, args) =>
        args.forall(isPurelyFunctional)
      case UnaryOp(_, operand) =>
        isPurelyFunctional(operand)
      case BinaryOp(lhs, _, rhs) =>
        isPurelyFunctional(lhs) && isPurelyFunctional(rhs)
      case Select(lhs, _) =>
        isPurelyFunctional(lhs)
      case Ternary(cond, thenBr, elseBr) =>
        isPurelyFunctional(cond) && isPurelyFunctional(thenBr) && isPurelyFunctional(elseBr)
      case Cast(expr, _) =>
        isPurelyFunctional(expr)
      case Sequence(stats, exprOpt) =>
        stats.forall(isPurelyFunctional) && exprOpt.exists(isPurelyFunctional)
      case Block(_) => false
      case LocalDef(_, _, rhs, isReassignable) =>
        !isReassignable && isPurelyFunctional(rhs)
      case VarAssig(_, _) => false
      case VarModif(_, _, _) => false
      case IfThenElse(_, _, _) => false
      case WhileLoop(_, _, _) => false
      case ForLoop(_, _, _, _, _) => false
      case ReturnStat(_) => false
      case PanicStat(_) => false
      case Assertion(_, _, _) => true
  }

  def isPurelyFunctional(funDef: FunDef)(implicit analysisContext: AnalysisContext): Boolean = {
    val stats = funDef.body.stats
    val initCond = stats.init.forall(stat => stat.isInstanceOf[LocalDef] && isPurelyFunctional(stat))
    def lastCond = stats.last match {
      case ReturnStat(Some(retVal)) if isPurelyFunctional(retVal) => true
      case _ => false
    }
    initCond && lastCond
  }

}
