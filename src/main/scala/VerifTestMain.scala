import compiler.Errors.ErrorReporter
import compiler.{FunctionsToInject, Mapper, TasksPipelines}
import compiler.TasksPipelines.{createErrorReporter, frontend}
import compiler.ctxcreator.ContextCreator
import compiler.desugarer.Desugarer
import compiler.io.{SourceFile, StringWriter}
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import compiler.renamer.Renamer
import compiler.typechecker.TypeChecker
import compiler.verification.{PathsGenerator, PathsVerifier, Z3Solver}

object VerifTestMain {

  def main(args: Array[String]): Unit = {
    TasksPipelines.verifier.apply(SourceFile(args(0)))
  }

}
