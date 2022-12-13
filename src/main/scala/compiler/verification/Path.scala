package compiler.verification

import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.prettyprinter.PrettyPrinter
import compiler.verification.Path.{PathElement, isControlFlowStat}
import lang.Types.PrimitiveType.BoolType

import scala.collection.mutable.ListBuffer

final case class Path(pathElems: List[PathElement], formulaToProve: Expr, descr: String) {
  require(formulaToProve.getType == BoolType)

  def assertAllTypesAreSet(): Unit = {
    for pathElem <- pathElems do {
      pathElem.assertAllTypesAreSet()
    }
    formulaToProve.assertAllTypesAreSet()
  }

  override def toString: String = {
    val prettyPrinter = new PrettyPrinter()
    val statsLines = pathElems.map(prettyPrinter.apply(_))
    val line = "-".repeat(statsLines.maxBy(_.length).length)
    val formulaToProveLine = " ==> " ++ prettyPrinter.apply(formulaToProve) ++ s"  [$descr]"
    (statsLines ++ List(line, formulaToProveLine)).mkString("\n")
  }

}

object Path {

  type PathElement = Assertion | LocalDef | VarAssig

  final class Builder() {
    private val stats = ListBuffer.empty[PathElement]
    private val varsCtx = new VarsCtx()

    def addPathElem(elem: PathElement): Builder = {
      stats.addOne(removeVars(elem))
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

    private def removeVars(pathElement: PathElement): PathElement = {
      pathElement match {
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
        case Assertion(formulaExpr, descr, isAssumed) =>
          Assertion(removeVars(formulaExpr), descr, isAssumed)
      }
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
        case Sequence(stats, exprOpt) =>
          Sequence(stats.map(removeVars), exprOpt.map(removeVars))
      }
      res.setType(expr.getType)
    }

    private def removeVars(statement: Statement): Statement = {
      statement match
        case expr: Expr =>
          removeVars(expr)
        case Block(stats) =>
          Block(removeVars(stats))
        case pathElement: PathElement =>
          removeVars(pathElement)
        case PanicStat(msg) =>
          PanicStat(removeVars(msg))
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
