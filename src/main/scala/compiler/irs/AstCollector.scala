package compiler.irs

import compiler.irs.Asts.*

object AstCollector {

  def collect[T](ast: Ast)(implicit pf: PartialFunction[Ast, List[T]]): List[T] = {
    val head = pf.applyOrElse(ast, _ => Nil)
    val tail = ast match {
      case _: Literal => Nil
      case _: VariableRef => Nil
      case Call(callee, args) =>
        collect(callee) ++ args.flatMap(collect)
      case Indexing(indexed, arg) =>
        collect(indexed) ++ collect(arg)
      case ArrayInit(_, size) =>
        collect(size)
      case FilledArrayInit(arrayElems) =>
        arrayElems.flatMap(collect)
      case StructInit(_, args) =>
        args.flatMap(collect)
      case UnaryOp(_, operand) =>
        collect(operand)
      case BinaryOp(lhs, _, rhs) =>
        collect(lhs) ++ collect(rhs)
      case Select(lhs, _) =>
        collect(lhs)
      case Ternary(cond, thenBr, elseBr) =>
        collect(cond) ++ collect(thenBr) ++ collect(elseBr)
      case Cast(expr, _) =>
        collect(expr)
      case Sequence(stats, expr) =>
        stats.flatMap(collect) ++ collect(expr)
      case Block(stats) =>
        stats.flatMap(collect)
      case LocalDef(_, _, rhs, _) =>
        collect(rhs)
      case VarAssig(lhs, rhs) =>
        collect(lhs) ++ collect(rhs)
      case VarModif(lhs, rhs, _) =>
        collect(lhs) ++ collect(rhs)
      case IfThenElse(cond, thenBr, elseBrOpt) =>
        collect(cond) ++ collect(thenBr) ++ elseBrOpt.flatMap(collect)
      case WhileLoop(cond, body, invariants) =>
        collect(cond) ++ collect(body) ++ invariants.flatMap(collect)
      case ForLoop(initStats, cond, stepStats, body, invariants) =>
        initStats.flatMap(collect) ++ collect(cond) ++ stepStats.flatMap(collect) ++ collect(body) ++ invariants.flatMap(collect)
      case ReturnStat(optVal) =>
        optVal.flatMap(collect)
      case PanicStat(msg) =>
        collect(msg)
      case Assertion(formulaExpr, _, _) =>
        collect(formulaExpr)
      case Source(defs) =>
        defs.flatMap(collect)
      case FunDef(_, params, _, body, precond, postcond) =>
        params.flatMap(collect) ++ collect(body) ++ precond.flatMap(collect) ++ postcond.flatMap(collect)
      case StructDef(_, fields) =>
        fields.flatMap(collect)
      case Param(_, _) => Nil
    }
    head ++ tail
  }

}
