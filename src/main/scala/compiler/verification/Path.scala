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

  override def toString: String = {
    val prettyPrinter = new PrettyPrinter()
    val statsLines = stats.map(prettyPrinter.apply(_))
    val line = "-".repeat(statsLines.maxBy(_.length).length)
    statsLines.mkString("\n") ++ "\n" ++ line ++ "\n" ++ prettyPrinter.apply(formulaToProve) ++ s"  [$descr]"
  }

}

object Path {

  final class Builder {
    private val stats = ListBuffer.empty[Statement]

    def addStat(stat: Statement): Builder = {
      require(!isControlFlowStat(stat), PrettyPrinter.prettyPrintStat(stat))
      stats.addOne(stat)
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
