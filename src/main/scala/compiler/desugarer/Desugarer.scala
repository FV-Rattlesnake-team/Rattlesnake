package compiler.desugarer

import compiler.irs.Asts.*
import compiler.prettyprinter.PrettyPrinter
import compiler.{AnalysisContext, CompilerStep, FunctionsToInject}
import lang.Operator.*
import lang.Operators
import lang.Types.PrimitiveType.*
import lang.Types.{ArrayType, UndefinedType}
import lang.SoftKeywords.Result

/**
 * Desugaring replaces:
 *  - `>`, `>=` ---> reversed
 *  - `x <= y` ---> `(x < y) || (x == y)`
 *  - `x != y` ---> `!(x == y)`
 *  - `VarModif`: `x += y` ---> `x = x + y`
 *  - `for` ---> `while`
 *  - `-x` ---> `0 - x`
 *  - `!x` ---> `when x then false else true`
 *  - `x && y` ---> `when x then y else false`
 *  - `x || y` ---> `when x then true else y`
 *  - `[x_1, ... , x_n]` ---> `val $0 = arr Int[n]; $0[0] = x_1; ... ; $0[n-1] = x_n; $0`
 */
final class Desugarer extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {
  private val uniqueIdGenerator = new UniqueIdGenerator()

  /*
  * =========================================================================
  * IMPORTANT: recursive calls to desugar(...) must be performed everywhere,
  * including on ASTs generated by another call to desugar (some expressions, 
  * e.g. `x >= y`, take several steps to be completely desugared)
  * =========================================================================
  */

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, ctx) = input
    val desugaredSources = sources.map(desugar(_)(ctx))
    desugaredSources.foreach(_.assertAllTypesAreSet())
    (desugaredSources, ctx)
  }

  private def desugar(src: Source)(implicit ctx: AnalysisContext): Source = Source(src.defs.map(desugar)).setName(src.getName)

  private def desugar(block: Block)(implicit ctx: AnalysisContext): Block = Block(block.stats.map(desugar))

  private def desugar(funDef: FunDef)(implicit ctx: AnalysisContext): FunDef = {
    val Block(bodyStats) = desugar(funDef.body)
    val newBodyStats = funDef.precond.map(formula =>
      desugar(Assertion(formula, PrettyPrinter.prettyPrintExpr(formula), isAssumed = true).setPositionSp(funDef.getPosition))
    ) ++ bodyStats
    val postcondWithRenaming = funDef.postcond.map(formula =>
      desugar(Assertion(formula, PrettyPrinter.prettyPrintExpr(formula))).setPositionSp(funDef.getPosition)
    )
    FunDef(funDef.funName, funDef.params.map(desugar), funDef.optRetType,
      addAssertsOnRetVals(Block(newBodyStats))(postcondWithRenaming), Nil, Nil)
  }

  private def desugar(structDef: StructDef)(implicit ctx: AnalysisContext): StructDef = {
    StructDef(structDef.structName, structDef.fields.map(desugar))
  }

  private def desugar(param: Param)(implicit ctx: AnalysisContext): Param = param

  private def desugar(localDef: LocalDef)(implicit ctx: AnalysisContext): LocalDef =
    LocalDef(localDef.localName, localDef.optType, desugar(localDef.rhs), localDef.isReassignable)

  private def desugar(varAssig: VarAssig)(implicit ctx: AnalysisContext): VarAssig = VarAssig(desugar(varAssig.lhs), desugar(varAssig.rhs))

  private def desugar(varModif: VarModif)(implicit ctx: AnalysisContext): VarAssig = {
    val VarModif(lhs, rhs, op) = varModif
    val desugaredLhs = desugar(lhs)
    val desugaredRhs = desugar(rhs)
    VarAssig(desugaredLhs, BinaryOp(desugaredLhs, op, desugaredRhs).setType(lhs.getType))
  }

  private def desugar(ifThenElse: IfThenElse)(implicit ctx: AnalysisContext): IfThenElse = {
    IfThenElse(desugar(ifThenElse.cond), desugar(ifThenElse.thenBr), ifThenElse.elseBrOpt.map(desugar))
  }

  private def desugar(whileLoop: WhileLoop)(implicit ctx: AnalysisContext): Statement = {
    val desugaredInvariants = whileLoop.invariants.map(invar => desugar(Assertion(invar, PrettyPrinter.prettyPrintExpr(invar)).setPositionSp(whileLoop.getPosition)))
    val newBodyStats = desugaredInvariants ++ whileLoop.body.asInstanceOf[Block].stats
    val loop = WhileLoop(desugar(whileLoop.cond), desugar(Block(newBodyStats)), Nil)
    if desugaredInvariants.isEmpty then loop else Block(loop :: desugaredInvariants)
  }

  private def desugar(forLoop: ForLoop)(implicit ctx: AnalysisContext): Block = {
    val body = Block(
      forLoop.body.stats ++ forLoop.stepStats
    )
    val stats: List[Statement] = forLoop.initStats :+ WhileLoop(forLoop.cond, body, forLoop.invariants)
    desugar(Block(stats))
  }

  private def desugar(returnStat: ReturnStat)(implicit ctx: AnalysisContext): ReturnStat = {
    ReturnStat(returnStat.optVal.map(desugar))
  }

  private def desugar(panicStat: PanicStat)(implicit ctx: AnalysisContext): PanicStat = {
    PanicStat(desugar(panicStat.msg))
  }

  private def desugar(assertion: Assertion)(implicit ctx: AnalysisContext): Assertion = {
    Assertion(desugar(assertion.formulaExpr), assertion.descr, assertion.isAssumed).setPositionSp(assertion.getPosition)
  }

  private def desugar(expr: Expr)(implicit ctx: AnalysisContext): Expr = {
    val desugared = expr match {
      case literal: Literal => literal
      case varRef: VariableRef => varRef
      case indexing: Indexing => Indexing(desugar(indexing.indexed), desugar(indexing.arg))
      case arrayInit: ArrayInit => ArrayInit(arrayInit.elemType, desugar(arrayInit.size))
      case structInit: StructInit => StructInit(structInit.structName, structInit.args.map(desugar))

      case call@Call(callee@VariableRef(name), args) =>
        val funInfo = ctx.functions(name)
        if (funInfo.precond.isEmpty && funInfo.postcond.isEmpty) {
          Call(desugar(callee), args.map(desugar)).setType(callee.getType)
        } else {
          val argsUids = for _ <- funInfo.sig.argTypes yield uniqueIdGenerator.next()
          val argsLocalDefsAndRefs = funInfo.sig.argTypes.indices.map { idx =>
            val uid = argsUids(idx)
            val tpe = funInfo.sig.argTypes(idx)
            (
              LocalDef(uid, Some(tpe), desugar(args(idx)), isReassignable = false),
              VariableRef(uid).setType(tpe)
            )
          }.toList
          val argsLocalDefinitions = argsLocalDefsAndRefs.map(_._1)
          val argsLocalReferences = argsLocalDefsAndRefs.map(_._2)
          val resultUid = uniqueIdGenerator.next()
          // can do optDef.get because only built-in functions can have None as a funDef, and built-ins do not have postconditions
          // can call zip because the typechecker checked the number of arguments
          val resultLocalRef = VariableRef(resultUid).setType(funInfo.sig.retType)
          val argsRenameMap = funInfo.optDef.get.params.map(_.paramName).zip(argsLocalReferences).toMap + (Result.str -> resultLocalRef)
          val newCall = Call(desugar(callee), argsLocalReferences).setType(funInfo.sig.retType)
          Sequence(
            argsLocalDefinitions ++
              funInfo.precond.map(formula =>
                desugar(Assertion(Replacer.replaceInExpr(desugar(formula), argsRenameMap), PrettyPrinter.prettyPrintExpr(formula))
                  .setPositionSp(call.getPosition))
              ) ++
              List(LocalDef(resultUid, Some(funInfo.sig.retType), newCall, isReassignable = false)) ++
              funInfo.postcond.map(formula =>
                desugar(Assertion(Replacer.replaceInExpr(desugar(formula), argsRenameMap), PrettyPrinter.prettyPrintExpr(formula), isAssumed = true)
                  .setPositionSp(call.getPosition))
              ),
            resultLocalRef
          )
        }

      case Call(_, _) => assert(false)

      // [x_1, ... , x_n] ---> explicit assignments
      case filledArrayInit@FilledArrayInit(arrayElems) =>
        val arrayType = filledArrayInit.getType.asInstanceOf[ArrayType]
        val elemType = arrayType.elemType
        val arrValId = uniqueIdGenerator.next()
        val arrValRef = VariableRef(arrValId).setType(arrayType)
        val arrInit = ArrayInit(elemType, IntLit(arrayElems.size)).setType(filledArrayInit.getType)
        val arrayValDefinition = LocalDef(arrValId, Some(arrayType), arrInit, isReassignable = false)
        val arrElemAssigStats = arrayElems.map(desugar).zipWithIndex.map {
          (elem, idx) => VarAssig(Indexing(arrValRef, IntLit(idx)).setType(UndefinedType), elem)
        }
        Sequence(arrayValDefinition :: arrElemAssigStats, arrValRef)

      case UnaryOp(operator, operand) =>
        val desugaredOperand = desugar(operand)
        operator match {
          case Minus if operand.getType == IntType => BinaryOp(IntLit(0), Minus, desugaredOperand)
          case Minus if operand.getType == DoubleType => BinaryOp(DoubleLit(0.0), Minus, desugaredOperand)
          case ExclamationMark => Ternary(desugaredOperand, BoolLit(false), BoolLit(true))
          case _ => UnaryOp(operator, desugaredOperand)
        }

      case BinaryOp(lhs, Equality, rhs) if lhs.getType == StringType => {
        val desugaredLhs = desugar(lhs)
        val desugaredRhs = desugar(rhs)
        Call(
          VariableRef(FunctionsToInject.stringEqualityMethodName).setType(UndefinedType),
          List(desugaredLhs, desugaredRhs)
        ).setType(BoolType)
      }

      case binaryOp: BinaryOp => {
        val desugaredLhs = desugar(binaryOp.lhs)
        val desugaredRhs = desugar(binaryOp.rhs)
        binaryOp.operator match {

          // x <= y ---> x <= y || x == y
          case LessOrEq => desugar(BinaryOp(
            BinaryOp(desugaredLhs, LessThan, desugaredRhs).setType(BoolType),
            Or,
            BinaryOp(desugaredLhs, Equality, desugaredRhs).setType(BoolType)
          ))

          // x > y ---> y < x  (and similar with >=)
          case GreaterThan => desugar(BinaryOp(desugaredRhs, LessThan, desugaredLhs))
          case GreaterOrEq => desugar(BinaryOp(desugaredRhs, LessOrEq, desugaredLhs))

          // x != y ---> !(x == y)
          case Inequality =>
            desugar(UnaryOp(ExclamationMark,
              BinaryOp(desugaredLhs, Equality, desugaredRhs).setType(BoolType)
            ).setType(BoolType))

          // x && y ---> when x then y else false
          case And => desugar(Ternary(desugaredLhs, desugaredRhs, BoolLit(false)))

          // x || y ---> when x then true else y
          case Or => desugar(Ternary(desugaredLhs, BoolLit(true), desugaredRhs))

          // nothing to desugar at top-level, only perform recursive calls
          case _ => BinaryOp(desugaredLhs, binaryOp.operator, desugaredRhs)
        }
      }
      case select: Select => Select(desugar(select.lhs), select.selected)

      // need to treat separately the case where one of the branches does not return (o.w. Java ASM crashes)
      case Ternary(cond, thenBr, elseBr) if thenBr.getType == NothingType || elseBr.getType == NothingType => {
        if (thenBr.getType == NothingType) {
          val ifStat = IfThenElse(cond, thenBr, None)
          desugar(Sequence(List(ifStat), elseBr))
        } else {
          val ifStat = IfThenElse(UnaryOp(ExclamationMark, cond).setType(BoolType), elseBr, None)
          desugar(Sequence(List(ifStat), thenBr))
        }
      }
      case Ternary(cond, thenBr, elseBr) => Ternary(desugar(cond), desugar(thenBr), desugar(elseBr))
      case Cast(expr, tpe) => Cast(desugar(expr), tpe)
      case Sequence(stats, expr) => Sequence(stats.map(desugar), desugar(expr))
    }
    desugared.setTypeOpt(expr.getTypeOpt)
  }

  private def desugar(statement: Statement)(implicit ctx: AnalysisContext): Statement = {
    // call appropriate method for each type of statement
    statement match
      case expr: Expr => desugar(expr)
      case block: Block => desugar(block)
      case localDef: LocalDef => desugar(localDef)
      case varAssig: VarAssig => desugar(varAssig)
      case varModif: VarModif => desugar(varModif)
      case ifThenElse: IfThenElse => desugar(ifThenElse)
      case whileLoop: WhileLoop => desugar(whileLoop)
      case forLoop: ForLoop => desugar(forLoop)
      case returnStat: ReturnStat => desugar(returnStat)
      case panicStat: PanicStat => desugar(panicStat)
      case assertion: Assertion => desugar(assertion)
  }

  private def desugar(topLevelDef: TopLevelDef)(implicit ctx: AnalysisContext): TopLevelDef = {
    topLevelDef match
      case funDef: FunDef => desugar(funDef)
      case structDef: StructDef => desugar(structDef)
  }

  private def addAssertsOnRetVals[S <: Statement](stat: S)(implicit rawAssertions: List[Assertion]): S = {
    if (rawAssertions.isEmpty) {
      stat
    } else {
      val resStat = stat match {
        case Block(stats) => Block(stats.map(addAssertsOnRetVals))
        case LocalDef(localName, optType, rhs, isReassignable) =>
          LocalDef(localName, optType, addAssertsOnRetVals(rhs), isReassignable)
        case VarAssig(lhs, rhs) => VarAssig(addAssertsOnRetVals(lhs), addAssertsOnRetVals(rhs))
        case VarModif(lhs, rhs, op) => VarModif(addAssertsOnRetVals(lhs), addAssertsOnRetVals(rhs), op)
        case IfThenElse(cond, thenBr, elseBrOpt) =>
          IfThenElse(addAssertsOnRetVals(cond), addAssertsOnRetVals(thenBr), elseBrOpt.map(addAssertsOnRetVals))
        case WhileLoop(cond, body, invariants) =>
          WhileLoop(addAssertsOnRetVals(cond), addAssertsOnRetVals(body), invariants.map(addAssertsOnRetVals))
        case ForLoop(initStats, cond, stepStats, body, invariants) =>
          ForLoop(
            initStats.map(addAssertsOnRetVals),
            addAssertsOnRetVals(cond),
            stepStats.map(addAssertsOnRetVals),
            addAssertsOnRetVals(body),
            invariants.map(addAssertsOnRetVals)
          )
        case PanicStat(msg) => PanicStat(addAssertsOnRetVals(msg))
        case Assertion(formulaExpr, descr, isAssumed) =>
          Assertion(addAssertsOnRetVals(formulaExpr), descr, isAssumed).setPositionSp(stat.getPosition)
        case literal: Literal => literal
        case variableRef: VariableRef => variableRef
        case Call(callee, args) => Call(addAssertsOnRetVals(callee), args.map(addAssertsOnRetVals))
        case Indexing(indexed, arg) => Indexing(addAssertsOnRetVals(indexed), addAssertsOnRetVals(arg))
        case ArrayInit(elemType, size) => ArrayInit(elemType, addAssertsOnRetVals(size))
        case FilledArrayInit(arrayElems) => FilledArrayInit(arrayElems.map(addAssertsOnRetVals))
        case StructInit(structName, args) => StructInit(structName, args.map(addAssertsOnRetVals))
        case UnaryOp(operator, operand) => UnaryOp(operator, addAssertsOnRetVals(operand))
        case BinaryOp(lhs, operator, rhs) =>
          BinaryOp(addAssertsOnRetVals(lhs), operator, addAssertsOnRetVals(rhs))
        case Select(lhs, selected) => Select(addAssertsOnRetVals(lhs), selected)
        case Ternary(cond, thenBr, elseBr) =>
          Ternary(addAssertsOnRetVals(cond), addAssertsOnRetVals(thenBr), addAssertsOnRetVals(elseBr))
        case Cast(expr, tpe) => Cast(addAssertsOnRetVals(expr), tpe)
        case Sequence(stats, expr) => Sequence(stats.map(addAssertsOnRetVals), addAssertsOnRetVals(expr))
        case retNone@ReturnStat(None) => retNone

        case retStat@ReturnStat(Some(retVal)) =>
          val uid = uniqueIdGenerator.next()
          val newLocalRef = VariableRef(uid).setType(retVal.getType)
          val renamedAssertions = rawAssertions.map(assertion =>
            Assertion(
              formulaExpr = Replacer.replaceInExpr(assertion.formulaExpr, Map(Result.str -> newLocalRef)),
              assertion.descr,
              assertion.isAssumed
            ).setPositionSp(retStat.getPosition)
          )
          Block(
            LocalDef(uid, retVal.getTypeOpt, retVal, isReassignable = false) :: renamedAssertions ++ List(ReturnStat(Some(newLocalRef)))
          )
      }
      assert(resStat.isInstanceOf[Expr] == stat.isInstanceOf[Expr])
      (stat, resStat) match {
        case (expr: Expr, resExpr: Expr) =>
          resExpr.setType(expr.getType)
        case _ => ()
      }
      resStat.asInstanceOf[S]
    }
  }

}
