package compiler.desugarer

import compiler.irs.Asts.{Ast, Source}
import compiler.{AnalysisContext, CompilerStep}
import compiler.irs.Asts.*
import lang.Operator.*
import lang.Operators

/**
 * Desugaring replaces:
 *  - `ValDef` -> `VarDef`
 *  - `>`, `>=` -> reversed
 *  - `x != y` -> `!(x == y)`
 *  - `VarModif`: `x += y` -> `x = x + y`
 *  - `for` -> `while`
 */
final class Desugarer extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, ctx) = input
    (sources.map(desugar), ctx)
  }

  private def desugar(src: Source): Source = Source(src.defs.map(desugar)).setName(src.getName)

  private def desugar(block: Block): Block = Block(block.stats.map(desugar))

  private def desugar(funDef: FunDef): FunDef = {
    FunDef(funDef.funName, funDef.params.map(desugar), funDef.optRetType, desugar(funDef.body))
  }

  private def desugar(structDef: StructDef): StructDef = {
    StructDef(structDef.structName, structDef.fields.map(desugar))
  }

  private def desugar(param: Param): Param = param

  private def desugar(valDef: ValDef): VarDef = VarDef(valDef.valName, valDef.optType, desugar(valDef.rhs))

  private def desugar(varDef: VarDef): VarDef = VarDef(varDef.varName, varDef.optType, desugar(varDef.rhs))

  private def desugar(varAssig: VarAssig): VarAssig = VarAssig(varAssig.lhs, varAssig.rhs)

  private def desugar(varModif: VarModif): VarAssig = {
    val VarModif(lhs, rhs, op) = varModif
    val desugaredLhs = desugar(lhs)
    val desugaredRhs = desugar(rhs)
    VarAssig(desugaredLhs, BinaryOp(desugaredLhs, Operators.assigOperators(op), desugaredRhs))
  }

  private def desugar(ifThenElse: IfThenElse): IfThenElse = {
    IfThenElse(desugar(ifThenElse.cond), desugar(ifThenElse.thenBr), ifThenElse.elseBrOpt.map(desugar))
  }

  private def desugar(whileLoop: WhileLoop): WhileLoop = {
    WhileLoop(desugar(whileLoop.cond), desugar(whileLoop.body))
  }

  private def desugar(forLoop: ForLoop): Block = {
    val body = Block(
      forLoop.body.stats ++ forLoop.stepStats
    )
    val stats: List[Statement] = forLoop.initStats :+ WhileLoop(forLoop.cond, body)
    Block(stats)
  }

  private def desugar(returnStat: ReturnStat): ReturnStat = ReturnStat(desugar(returnStat.value))

  private def desugar(panicStat: PanicStat): PanicStat = PanicStat(desugar(panicStat.msg))

  private def desugar(expr: Expr): Expr = {
    val desugared = expr match {
      case literal: Literal => literal
      case varRef: VariableRef => varRef
      case call: Call => Call(desugar(call.callee), call.args.map(desugar))
      case indexing: Indexing => Indexing(desugar(indexing.indexed), desugar(indexing.arg))
      case arrayInit: ArrayInit => ArrayInit(arrayInit.elemType, desugar(arrayInit.size))
      case structInit: StructInit => StructInit(structInit.structName, structInit.args.map(desugar))
      case unaryOp: UnaryOp => UnaryOp(unaryOp.operator, desugar(unaryOp.operand))
      case binaryOp: BinaryOp => {
        val desugaredLhs = desugar(binaryOp.lhs)
        val desugaredRhs = desugar(binaryOp.rhs)
        binaryOp.operator match
          case GreaterThan => BinaryOp(desugaredRhs, LessThan, desugaredLhs)
          case GreaterOrEq => BinaryOp(desugaredRhs, LessOrEq, desugaredLhs)
          case Inequality => UnaryOp(ExclamationMark, BinaryOp(desugaredRhs, Equality, desugaredLhs))
          case _ => BinaryOp(desugaredLhs, binaryOp.operator, desugaredRhs)
      }
      case select: Select => Select(desugar(select.lhs), select.selected)
    }
    desugared.setTypeOpt(expr.getTypeOpt)
    desugared
  }

  private def desugar(statement: Statement): Statement = {
    statement match
      case expr: Expr => desugar(expr)
      case block: Block => desugar(block)
      case valDef: ValDef => desugar(valDef)
      case varDef: VarDef => desugar(varDef)
      case varAssig: VarAssig => desugar(varAssig)
      case varModif: VarModif => desugar(varModif)
      case ifThenElse: IfThenElse => desugar(ifThenElse)
      case whileLoop: WhileLoop => desugar(whileLoop)
      case forLoop: ForLoop => desugar(forLoop)
      case returnStat: ReturnStat => desugar(returnStat)
      case panicStat: PanicStat => desugar(panicStat)
  }

  private def desugar(topLevelDef: TopLevelDef): TopLevelDef = {
    topLevelDef match
      case funDef: FunDef => desugar(funDef)
      case structDef: StructDef => desugar(structDef)
  }

}