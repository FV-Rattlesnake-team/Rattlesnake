package compiler.desugarer

import compiler.irs.Asts.*

object Replacer {

  /**
   * Replace variables in `expr` according to the `renameMap`
   */
  def replaceInExpr[E <: Expr](expr: E, renameMap: Map[String, Expr]): E = {
    replaceInExprImpl(expr)(renameMap)
  }

  private def replaceInExprImpl[E <: Expr](expr: E)(implicit replMap: Map[String, Expr]): E = {
    val resExpr = expr match {
      case _: Literal => expr
      case varRef@VariableRef(name) => replMap.getOrElse(name, varRef)
      case Call(callee, args) => Call(replaceInExprImpl(callee), args.map(replaceInExprImpl))
      case Indexing(indexed, arg) => Indexing(replaceInExprImpl(indexed), replaceInExprImpl(arg))
      case ArrayInit(elemType, size) => ArrayInit(elemType, replaceInExprImpl(size))
      case FilledArrayInit(arrayElems) => FilledArrayInit(arrayElems.map(replaceInExprImpl))
      case StructInit(structName, args) => StructInit(structName, args.map(replaceInExprImpl))
      case UnaryOp(operator, operand) => UnaryOp(operator, replaceInExprImpl(operand))
      case BinaryOp(lhs, operator, rhs) => BinaryOp(replaceInExprImpl(lhs), operator, replaceInExprImpl(rhs))
      case Select(lhs, selected) => Select(replaceInExprImpl(lhs), selected)
      case Ternary(cond, thenBr, elseBr) => Ternary(replaceInExprImpl(cond), replaceInExprImpl(thenBr), replaceInExprImpl(elseBr))
      case Cast(expr, tpe) => Cast(replaceInExprImpl(expr), tpe)
      case Sequence(stats, expr) => Sequence(stats.map(renameInStat), replaceInExprImpl(expr))
    }
    resExpr.setTypeOpt(expr.getTypeOpt)
    resExpr.asInstanceOf[E]
  }

  private def renameInStat[S <: Statement](stat: S)(implicit replMap: Map[String, Expr]): S = {
    val resStat = stat match {
      case expr: Expr => replaceInExprImpl(expr)
      case Block(stats) => Block(stats.map(renameInStat))
      case VarAssig(lhs, rhs) => VarAssig(replaceInExprImpl(lhs), replaceInExprImpl(rhs))
      case VarModif(lhs, rhs, op) => VarModif(replaceInExprImpl(lhs), replaceInExprImpl(rhs), op)
      case IfThenElse(cond, thenBr, elseBrOpt) => IfThenElse(replaceInExprImpl(cond), renameInStat(thenBr), elseBrOpt.map(renameInStat))
      case WhileLoop(cond, body, invariants) => WhileLoop(replaceInExprImpl(cond), renameInStat(body), invariants.map(replaceInExprImpl))
      case ForLoop(initStats, cond, stepStats, body, invariants) =>
        ForLoop(initStats.map(renameInStat), replaceInExprImpl(cond), stepStats.map(renameInStat), renameInStat(body), invariants.map(replaceInExprImpl))
      case ReturnStat(optVal) => ReturnStat(optVal.map(replaceInExprImpl))
      case Assertion(formulaExpr, descr, isAssumed) =>
        Assertion(replaceInExprImpl(formulaExpr), descr, isAssumed).setPositionSp(stat.getPosition)
      case _: PanicStat => stat
      case _: LocalDef => assert(false)
    }
    resStat.asInstanceOf[S]
  }

}
