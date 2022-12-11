package compiler.verification

import smtlib.trees.Commands.Script
import smtlib.trees.Terms.Term

trait Solver {
  
  def check(smtScript: Script, timeoutSec: Int, comments: List[String], idx: Int): Solver.Result

}

object Solver {

  sealed trait Result
  case object Sat extends Result
  case object Unsat extends Result
  final case class Timeout(timeoutSec: Int) extends Result
  final case class Error(msg: String) extends Result

}
