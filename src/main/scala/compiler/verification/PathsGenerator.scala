package compiler.verification

import compiler.{AnalysisContext, CompilerStep}
import compiler.irs.Asts.Source
import compiler.irs.Asts.*
import compiler.verification.Path
import scala.collection.mutable.ListBuffer

final class PathsGenerator extends CompilerStep[(List[Source], AnalysisContext), List[Path]] {

  override def apply(input: (List[Source], AnalysisContext)): List[Path] = {
    val (sources, _) = input
    val paths = ListBuffer.empty[Path]
    for {
      src <- sources
      df <- src.defs
    } do {
      df match
        case _: StructDef => ()
        case FunDef(_, _, _, body, _, _) =>
          val pathBuilder = new Path.Builder()
          generatePaths(body, List(pathBuilder))(paths)
    }
    val res = paths.toList
    res.foreach(_.assertAllTypesAreSet())
    res
  }

  private def generatePaths(
                             statement: Statement,
                             pathBuilders: List[Path.Builder]
                           )(implicit paths: ListBuffer[Path]): List[Path.Builder] = {

    def pathBuildersUpdated: List[Path.Builder] = pathBuilders.map(_.addStat(statement))

    statement match
      case Sequence(stats, exprOpt) =>
        generatePaths(stats ++ exprOpt, pathBuilders)
      case Call(_, args) =>
        generatePaths(args, pathBuilders)
      case _: (Literal | VariableRef) =>
        pathBuilders
      case Indexing(indexed, arg) =>
        generatePaths(List(indexed, arg), pathBuilders)
      case ArrayInit(_, size) =>
        generatePaths(size, pathBuilders)
      case FilledArrayInit(arrayElems) =>
        generatePaths(arrayElems, pathBuilders)
      case StructInit(_, args) =>
        generatePaths(args, pathBuilders)
      case UnaryOp(_, operand) =>
        generatePaths(operand, pathBuilders)
      case BinaryOp(lhs, _, rhs) =>
        generatePaths(List(lhs, rhs), pathBuilders)
      case Select(lhs, _) =>
        generatePaths(lhs, pathBuilders)
      case Ternary(cond, thenBr, elseBr) =>
        generatePaths(List(cond, thenBr, elseBr), pathBuilders)
      case Cast(expr, _) =>
        generatePaths(expr, pathBuilders)
      case Block(stats) =>
        generatePaths(stats, pathBuilders)
      case _: LocalDef =>
        pathBuildersUpdated
      case _: Assignment =>
        pathBuildersUpdated
      case IfThenElse(_, thenBr, elseBrOpt) =>
        val elsePathBuilders = pathBuilders.map(_.copied)
        generatePaths(thenBr, pathBuilders) ++ elseBrOpt.map(generatePaths(_, elsePathBuilders)).getOrElse(Nil)
      case WhileLoop(_, body, _) =>
        generatePaths(body, List(new Path.Builder()))
        List(new Path.Builder())
      case _: ReturnStat =>
        pathBuilders
      case _: PanicStat =>
        pathBuildersUpdated
      case Assertion(formulaExpr, descr, isAssumed) =>
        if (!isAssumed) {
          for pathB <- pathBuilders do {
            paths.addOne(pathB.builtWith(formulaExpr, descr))
          }
        }
        pathBuildersUpdated
      case _: ForLoop => assert(false)
  }

  private def generatePaths(
                             statements: List[Statement],
                             pathBuilders: List[Path.Builder]
                           )(implicit paths: ListBuffer[Path]): List[Path.Builder] = {
    statements.foldLeft(pathBuilders){ (pBuilders, currStat) =>
      generatePaths(currStat, pBuilders)
    }
  }

}
