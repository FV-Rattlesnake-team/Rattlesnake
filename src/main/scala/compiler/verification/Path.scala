package compiler.verification

import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.prettyprinter.PrettyPrinter
import compiler.verification.Path.isControlFlowStat
import lang.Types.PrimitiveType.BoolType

import scala.collection.mutable.ListBuffer

final case class Path(stats: List[Statement], formulaToProve: Expr, descr: String){
  stats.foreach(stat => require(!isControlFlowStat(stat), PrettyPrinter.prettyPrintStat(stat)))
  require(formulaToProve.getType == BoolType)
  
  def assertAllTypesAreSet(): Unit = {
    for stat <- stats do {
      stat match
        case expr: Expr => expr.assertAllTypesAreSet()
        case _ => ()
    }
  }

  override def toString: String = {
    val prettyPrinter = new PrettyPrinter()
    val statsLines = stats.map(prettyPrinter.apply(_))
    val line = "-".repeat(statsLines.maxBy(_.length).length)
    statsLines.mkString("\n") ++ "\n" ++ line ++ "\n ==> " ++ prettyPrinter.apply(formulaToProve) ++ s"  [$descr]"
  }

}

object Path {

  final class Builder() {
    private val stats = ListBuffer.empty[Statement]
    private val varsCtx = new VarsCtx()

    def addStat(stat: Statement): Builder = {
      require(!isControlFlowStat(stat), PrettyPrinter.prettyPrintStat(stat))
      stats.addOne(removeVars(stat))
      this
    }

    def copied: Builder = {
      val copy = new Builder()
      copy.stats.addAll(stats)
      copy
    }

    def builtWith(formulaToProve: Expr, descr: String): Path = {
      require(formulaToProve.getType == BoolType)
      Path(stats.toList, formulaToProve, descr)
    }

    private def removeVars(stats: List[Statement]): List[Statement] = {
      stats.map(removeVars)
    }

    private def removeVars(expr: Expr): Expr = {
      val res = expr match {
        case literal: Asts.Literal =>
          literal
        case VariableRef(name) =>
          VariableRef(varsCtx.nameFor(name))
        case Call(callee, args) =>
          Call(callee, args.map(removeVars))
        case Indexing(indexed, arg) =>
          Indexing(removeVars(indexed), removeVars(arg))
        case ArrayInit(elemType, size) =>
          ArrayInit(elemType, removeVars(size))
        case FilledArrayInit(arrayElems) =>
          FilledArrayInit(arrayElems.map(removeVars))
        case StructInit(structName, args) =>
          StructInit(structName, args.map(removeVars))
        case UnaryOp(operator, operand) =>
          UnaryOp(operator, removeVars(operand))
        case BinaryOp(lhs, operator, rhs) =>
          BinaryOp(removeVars(lhs), operator, removeVars(rhs))
        case Select(lhs, selected) =>
          Select(removeVars(lhs), selected)
        case Ternary(cond, thenBr, elseBr) =>
          Ternary(removeVars(cond), removeVars(thenBr), removeVars(elseBr))
        case Cast(expr, tpe) =>
          Cast(removeVars(expr), tpe)
        case Sequence(stats, expr) =>
          Sequence(stats.map(removeVars), removeVars(expr))
      }
      res.setType(expr.getType)
    }

    private def removeVars(statement: Statement): Statement = {
      statement match
        case expr: Expr =>
          removeVars(expr)
        case Block(stats) =>
          Block(removeVars(stats))
        case LocalDef(localName, optType, rhs, _) =>
          val transformedRhs = removeVars(rhs)
          val newName = varsCtx.newNameFor(localName)
          LocalDef(newName, optType, transformedRhs, isReassignable = false)
        case VarAssig(VariableRef(name), rhs) =>
          val transformedRhs = removeVars(rhs)
          val newName = varsCtx.newNameFor(name)
          LocalDef(newName, Some(rhs.getType), transformedRhs, isReassignable = false)
        case VarAssig(lhs, rhs) =>
          VarAssig(removeVars(lhs), removeVars(rhs))
        case PanicStat(msg) =>
          PanicStat(removeVars(msg))
        case Assertion(formulaExpr, descr, isAssumed) =>
          Assertion(removeVars(formulaExpr), descr, isAssumed)
        case _: (VarModif | IfThenElse | WhileLoop | ForLoop | ReturnStat) =>
          assert(false)
    }

  }

  private def isControlFlowStat(statement: Statement): Boolean = {
    statement match

      case _: IfThenElse => true
      case _: WhileLoop => true
      case _: ForLoop => true
      case _: Block => true

      case _: Expr => false
      case _: LocalDef => false
      case _: Asts.Assignment => false
      case _: ReturnStat => false
      case _: PanicStat => false
      case _: Assertion => false
  }

}
