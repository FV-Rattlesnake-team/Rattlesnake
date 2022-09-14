package compiler

import compiler.Errors.ErrorReporter
import compiler.io.SourceFile
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import org.junit.Assert.assertEquals
import org.junit.Test

class ParseAndReprintTests {

  @Test
  def parseAndReprintTest(): Unit = {
    val er = new ErrorReporter(System.err.println)
    val formatter = new Lexer(er) andThen new Parser(er) andThen new PrettyPrinter(indentGranularity = 4)
    val file = SourceFile("src/test/res/geometry.rsn")
    val actualRes = formatter.apply(file)
    val expectedRes = filterOutCommentLines(file.content.get)
    assertEquals(expectedRes, actualRes)
  }

  private def filterOutCommentLines(str: String): String = {
    str.lines().filter { line =>
      !line.trim.startsWith("//")
    }.toArray.mkString("\n")
  }

}