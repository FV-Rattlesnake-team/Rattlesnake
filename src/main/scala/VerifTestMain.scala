import compiler.Errors.ErrorReporter
import compiler.{FunctionsToInject, Mapper}
import compiler.TasksPipelines.{createErrorReporter, frontend}
import compiler.ctxcreator.ContextCreator
import compiler.desugarer.Desugarer
import compiler.io.{SourceFile, StringWriter}
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import compiler.typechecker.TypeChecker
import compiler.verification.PathsGenerator

object VerifTestMain {

  def main(args: Array[String]): Unit = {
    val er = new ErrorReporter(println)
    val pipeline = {
      new Lexer(er)
        .andThen(new Parser(er))
        .andThen(Mapper(List(_)))
        .andThen(new ContextCreator(er, FunctionsToInject.functionsToInject))
        .andThen(new TypeChecker(er))
        .andThen(new Desugarer(desugarStringEq = false))
        .andThen(new PathsGenerator())
    }
    val paths = pipeline.apply(SourceFile(args(0)))
    for path <- paths do {
      println(path)
      println
    }
  }

}
