package compiler.verification

import compiler.CompilationStep.Verification
import compiler.{CompilerStep, Position}
import compiler.Errors.{Err, ErrorReporter, errorsExitCode}
import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.verification.PathsVerifier.Score
import compiler.verification.solver.Solver
import compiler.verification.solver.Solver.*
import lang.Operator.*
import lang.{Operator, Types}
import lang.Types.PrimitiveType.*
import lang.Types.{ArrayType, PrimitiveType, StructType, Type, UndefinedType}
import smtlib.theories.Core.{BoolSort, Equals, False, Implies, Not, True}
import smtlib.theories.{Core, Ints}
import smtlib.theories.Ints.IntSort
import smtlib.trees.Terms.*
import smtlib.trees.Commands.{CheckSatAssuming, Command, DeclareConst, PropLiteral, Script, Assert as AssertCmd}

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

final class PathsVerifier(
                           solver: Solver,
                           timeoutSec: Int,
                           errorReporter: ErrorReporter,
                           logger: String => Unit
                         ) extends CompilerStep[List[Path], Score] {

  private val atomicIntUid = new AtomicInteger(0)

  private def nextNameForNewVar(): String = {
    "%" ++ atomicIntUid.incrementAndGet().toString
  }

  override def apply(paths: List[Path]): Score = {
    solver.initialize()
    var correctCnt = 0
    for ((path, idx) <- paths.zipWithIndex) do {
      val base1Idx = idx + 1

      def genPrintableReport(msg: String, assigStr: String = ""): String = {
        s"$base1Idx - $msg : ${path.descr} $assigStr"
      }

      logger(
        verify(path, base1Idx, errorReporter) match
          case Solver.Sat(varsAssigDescr) => {
            genPrintableReport("FAILURE", varsAssigDescr)
          }
          case Solver.Unsat =>
            correctCnt += 1
            genPrintableReport("success")
          case Solver.Timeout(timeoutSec) => {
            genPrintableReport(s"TIMEOUT ($timeoutSec s)")
          }
          case Error(msg) => {
            genPrintableReport("ERROR: " ++ msg)
          }
      )
    }
    errorReporter.displayAndTerminateIfErrors()
    Score(correctCnt, paths.size)
  }

  private final class ErrorFlag {
    private var flag: Boolean = false

    def set(): Unit = {
      flag = true
    }

    def isSet: Boolean = flag
  }

  private def verify(path: Path, idx: Int, errorReporter: ErrorReporter): Solver.Result = {
    val Path(stats, formulaToProve, descr) = path
    val errorFlag = new ErrorFlag()
    val additionalVarsBuffer = ListBuffer.empty[(String, Type)]
    val additionalFormulasBuffer = ListBuffer.empty[Term]
    val assumedFormulas = stats.flatMap(generateFormulas(_)(errorReporter, errorFlag, additionalVarsBuffer, additionalFormulasBuffer))
    val convertedFormulaToProve = transformExpr(formulaToProve)(errorReporter, errorFlag, additionalVarsBuffer, additionalFormulasBuffer)
    if (errorFlag.isSet) {
      Solver.Error("solver error")
    } else {
      val vars = (formulaToProve :: stats).flatMap(allVariables).toMap.toList // eliminate duplicates
      val varsDecls = {
        (vars ++ additionalVarsBuffer)
          .map((name, tpe) => DeclareConst(SSymbol(name), convertType(tpe)(errorReporter, errorFlag)))
      }
      val allAssumpt = assumedFormulas ++ additionalFormulasBuffer
      val assumptFormula = {
        allAssumpt match
          case Nil => True()
          case single :: Nil => single
          case _ => Core.And(allAssumpt)
      }
      val implication = Implies(
        assumptFormula,
        convertedFormulaToProve
      )
      val script = Script(varsDecls :+ AssertCmd(Not(implication)))
      solver.check(script, timeoutSec, s"target: $descr\n\n$path", idx)
    }
  }

  // this function also considers the function names, which should not be a problem since these constants are just ignored
  private def allVariables(ast: Ast): List[(String, Type)] = {
    ast.collectLs {
      case varRef@VariableRef(name) => {
        List((name, varRef.getType))
      }
      case LocalDef(localName, optType, rhs, _) => {
        val tpe = optType.getOrElse(rhs.getType)
        List((localName, tpe))
      }
    }
  }

  private def generateFormulas(statement: Statement)(
    implicit errorReporter: ErrorReporter,
    errorFlag: ErrorFlag,
    additVarsBuffer: ListBuffer[(String, Type)],
    additFormulasBuffer: ListBuffer[Term]
  ): List[Term] = {

    extension (term: Term) def toSingletonList: List[Term] = List(term)

    statement match
      case _: Asts.Literal => Nil
      case _: VariableRef => Nil
      case _: Call => Nil
      case _: Indexing => Nil
      case _: ArrayInit =>
        reportUnsupported("array initialization", statement.getPosition)
      case _: FilledArrayInit =>
        reportUnsupported("array initialization", statement.getPosition)
      case _: StructInit =>
        reportUnsupported("struct initialization", statement.getPosition)
      case _: UnaryOp => Nil
      case _: BinaryOp => Nil
      case _: Select => Nil
      case Ternary(cond, thenBr, elseBr) =>
        generateFormulas(cond) ++ generateFormulas(thenBr) ++ generateFormulas(elseBr)
      case _: Cast => Nil
      case Sequence(_, exprOpt) =>
        exprOpt.flatMap(generateFormulas).toList
      case Block(stats) =>
        stats.flatMap(generateFormulas)
      case LocalDef(localName, _, rhs, _) =>
        Equals(qid(localName), transformExpr(rhs)).toSingletonList
      case Assertion(formulaExpr, _, _) => // assumed in this path since it appears in the path
        transformExpr(formulaExpr).toSingletonList
      case _: (VarAssig | VarModif | ForLoop | ReturnStat | IfThenElse | WhileLoop | PanicStat) =>
        throw new AssertionError(s"unexpected $statement")
  }

  private def transformExpr(expr: Expr)(
    implicit er: ErrorReporter,
    errorFlag: ErrorFlag,
    additVarsBuffer: ListBuffer[(String, Type)],
    additFormulasBuffer: ListBuffer[Term]
  ): Term = {
    if (expr.getType.subtypeOf(NothingType)) {
      reportUnsupported(s"expression with return type $NothingType", expr.getPosition)
    } else {
      expr match
        case IntLit(value) =>
          Ints.NumeralLit(value)
        case dLit@DoubleLit(_) =>
          reportUnsupported(DoubleType.str, dLit.getPosition)
        case cLit@CharLit(_) =>
          reportUnsupported(CharType.str, cLit.getPosition)
        case BoolLit(true) =>
          True()
        case BoolLit(false) =>
          False()
        case sLit@StringLit(_) =>
          reportUnsupported(StringType.str, sLit.getPosition)
        case VariableRef(name) =>
          qid(name)
        case call: Call =>
          val newVarName = nextNameForNewVar()
          additVarsBuffer.addOne(newVarName -> call.getType)
          qid(newVarName)  // variable with no constraint, since there is no postcondition
        case indexing@Indexing(_, _) =>
          reportUnsupported("array indexing", indexing.getPosition)
        case arrInit@ArrayInit(_, _) =>
          reportUnsupported("array initialization", arrInit.getPosition)
        case arrInit@FilledArrayInit(_) =>
          reportUnsupported("array initialization", arrInit.getPosition)
        case structInit@StructInit(_, _) =>
          reportUnsupported("struct initialization", structInit.getPosition)
        case UnaryOp(ExclamationMark, operand) =>
          Not(transformExpr(operand))
        case UnaryOp(Minus, operand) if operand.getType.subtypeOf(IntType) =>
          Ints.Neg(transformExpr(operand))
        case unOp@UnaryOp(Minus, operand) if operand.getType.subtypeOf(DoubleType) =>
          reportUnsupported(DoubleType.str, unOp.getPosition)
        case unOp@UnaryOp(Sharp, _) =>
          reportUnsupported(s"${Sharp.str}", unOp.getPosition)
        case unaryOp: UnaryOp =>
          throw new AssertionError(s"unexpected: $unaryOp")
        case binOp@BinaryOp(lhs, operator, rhs) => {
          if (operator == Equality) {
            Equals(transformExpr(lhs), transformExpr(rhs))
          } else if (lhs.getType.subtypeOf(BoolType)) {
            val transformedLhs = transformExpr(lhs)
            val transformedRhs = transformExpr(rhs)
            operator match {
              case And =>
                Core.And(transformedLhs, transformedRhs)
              case Or =>
                Core.Or(transformedLhs, transformedRhs)
              case _ =>
                assert(false)
            }
          } else if (lhs.getType.subtypeOf(IntType)) {
            convertOperatorToIntsTheoryOps(operator, lhs, rhs)
          } else {
            reportUnsupported(lhs.getType.toString, binOp.getPosition)
          }
        }
        case sel@Select(_, _) =>
          reportUnsupported("select", sel.getPosition)
        case ternary@Ternary(cond, thenBr, elseBr) =>
          val newVarName = nextNameForNewVar()
          val resVar = qid(newVarName)
          val condTransformed = transformExpr(cond)
          val thenTerm = Core.And(condTransformed, Equals(resVar, transformExpr(thenBr)))
          val elseTerm = Core.And(Not(condTransformed), Equals(resVar, transformExpr(elseBr)))
          additFormulasBuffer.addOne(Core.Or(thenTerm, elseTerm))
          additVarsBuffer.addOne(newVarName -> ternary.getType)
          resVar
        case cast@Cast(_, _) =>
          reportUnsupported("cast", cast.getPosition)
        case Sequence(_, Some(expr)) =>
          transformExpr(expr)
        case sequence: Sequence =>
          reportUnsupported("no value returned", sequence.getPosition)
    }
  }

  private def convertType(tpe: Type)(implicit errorReporter: ErrorReporter, errorFlag: ErrorFlag): Sort = {
    tpe match
      case IntType =>
        IntSort()
      case DoubleType =>
        reportUnsupported(tpe.toString, None)
      case CharType =>
        reportUnsupported(tpe.toString, None)
      case BoolType =>
        BoolSort()
      case StringType =>
        reportUnsupported(tpe.toString, None)
      case StructType(_) =>
        reportUnsupported(tpe.toString, None)
      case ArrayType(_) =>
        reportUnsupported(tpe.toString, None)
      case VoidType =>
        assert(false)
      case NothingType =>
        assert(false)
      case UndefinedType =>
        assert(false)
  }

  private def convertOperatorToIntsTheoryOps(
                                              operator: Operator,
                                              lhs: Expr,
                                              rhs: Expr
                                            )(
                                              implicit errorReporter: ErrorReporter,
                                              errorFlag: ErrorFlag,
                                              additVarsBuffer: ListBuffer[(String, Type)],
                                              additFormulasBuffer: ListBuffer[Term]
                                            ): Term = {
    val transformedLhs = transformExpr(lhs)
    val transformedRhs = transformExpr(rhs)
    operator match {
      case Plus =>
        Ints.Add(transformedLhs, transformedRhs)
      case Minus =>
        Ints.Sub(transformedLhs, transformedRhs)
      case Times =>
        Ints.Mul(transformedLhs, transformedRhs)
      case Div =>
        Ints.Div(transformedLhs, transformedRhs)
      case Modulo =>
        Ints.Mod(transformedLhs, transformedRhs)
      case Equality =>
        Equals(transformedLhs, transformedRhs)
      case Inequality =>
        Not(Equals(transformedLhs, transformedRhs))
      case LessThan =>
        Ints.LessThan(transformedLhs, transformedRhs)
      case LessOrEq =>
        Ints.LessEquals(transformedLhs, transformedRhs)
      case GreaterThan =>
        Ints.GreaterThan(transformedLhs, transformedRhs)
      case GreaterOrEq =>
        Ints.GreaterEquals(transformedLhs, transformedRhs)
      case _ => throw new AssertionError(s"unexpected: $operator")
    }
  }

  private def qid(name: String): QualifiedIdentifier = {
    QualifiedIdentifier(SimpleIdentifier(SSymbol(name)))
  }

  private def reportUnsupported(
                                 msg: String, posOpt: Option[Position]
                               )(implicit errorReporter: ErrorReporter, errorFlag: ErrorFlag): Null = {
    errorReporter.push(Err(Verification, "Not supported: " ++ msg, posOpt))
    errorFlag.set()
    null
  }

}

object PathsVerifier {
  
  final case class Score(passed: Int, total: Int){
    require(passed <= total)
    
    def allPassed: Boolean = (passed == total)

    override def toString: String = s"$passed/$total"
  }
  
}
