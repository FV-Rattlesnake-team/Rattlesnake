package compiler.verification.solver

import compiler.verification.solver.Solver
import smtlib.trees.Commands.Script
import smtlib.trees.Terms.Term

import scala.util.Try

trait Solver {
  
  def initialize(): Unit
  def check(smtScript: Script, timeoutSec: Int, comments: String, idx: Int): Solver.Result

}

object Solver {

  sealed trait Result
  final case class Sat(varsAssigOpt: Try[Map[String, String]]) extends Result
  case object Unsat extends Result
  final case class Timeout(timeoutSec: Int) extends Result
  final case class Error(msg: String) extends Result

}
