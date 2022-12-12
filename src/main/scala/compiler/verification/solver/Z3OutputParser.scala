package compiler.verification.solver

import lang.Operator.{ClosingParenthesis, Minus, OpeningParenthesis}
import smtlib.trees.CommandsResponses.Success

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Failure, Try}
import scala.collection.mutable

object Z3OutputParser {

  def parse(lines: List[String]): Try[Map[String, String]] = {

    def fail: Try[Map[String, String]] = {
      Failure(new Exception(s"unable to parse z3 response"))
    }

    def parseVariableLine(line: String): String = {
      line.trim.replace("(define-fun ", "").takeWhile(!_.isWhitespace)
    }

    def parseValueLine(line: String): String = {
      line.trim.init  // drop terminal parenthesis
    }

    if (lines.size >= 2 && lines.head.trim == "(" && lines.last.trim == ")") {
      val linesIter = lines.iterator
      val assig = mutable.Map.empty[String, String]
      linesIter.next()  // drop first line
      while (linesIter.hasNext){
        val curr = linesIter.next()
        if (!linesIter.hasNext){
          return scala.util.Success(assig.toMap)
        }
        val variable = parseVariableLine(curr)
        val value = if (linesIter.hasNext){
          parseValueLine(linesIter.next())
        } else {
          return fail
        }
        assig.put(variable, value)
      }
      assert(false)
    } else {
      fail
    }
  }

}
