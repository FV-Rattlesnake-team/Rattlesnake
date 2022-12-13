package compiler.verification.solver

import compiler.verification.solver.Solver
import compiler.verification.solver.Solver.*
import smtlib.trees.Commands.{CheckSat, GetModel, Script}
import smtlib.trees.{Commands, Terms}
import util.IO

import java.io.{BufferedReader, FileWriter, InputStreamReader}
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try, Using}

final class Z3Solver(outputDir: java.nio.file.Path) extends Solver {
  private val z3ExecName = "z3"

  {
    val dir = outputDir.toFile
    if (dir.exists()) {
      clearDir()
    } else {
      dir.mkdir()
    }
  }

  private def nextFilepath(idx: Int) = {
    outputDir.resolve(s"z3input_$idx.smt")
  }

  override def check(smtScript: Commands.Script, timeoutSec: Int, comments: String, idx: Int): Result = {
    val filepath = nextFilepath(idx)
    writeFile(smtScript, filepath, comments).map { _ =>
      val timeoutOption = s"-T:$timeoutSec"
      runCommand(List(z3ExecName, timeoutOption, filepath.toString), timeoutSec)
    }.recover(exc => Error(exc.getMessage)).get
  }

  private def clearDir(): Unit = {
    for file <- outputDir.toFile.listFiles() do {
      IO.deleteRecursively(file)
    }
  }

  private def writeFile(script: Script, tmpFilePath: java.nio.file.Path, comments: String): Try[Unit] = {
    val file = tmpFilePath.toFile
    file.createNewFile()
    Using(new FileWriter(file)) { writer =>
      writeComments(comments, writer)
      val printer = smtlib.printer.RecursivePrinter
      printer.printScript(script, writer)
      printer.printCommand(CheckSat(), writer)
      printer.printCommand(GetModel(), writer)
    }
  }

  private def writeComments(comments: String, writer: FileWriter): Unit = {
    for line <- io.Source.fromString(comments).getLines() do {
      writer.write(s"; $line\n")
    }
  }

  private def runCommand(cmd: List[String], timeoutSec: Int): Result = {

    // adapted from https://stackoverflow.com/questions/5711084/java-runtime-getruntime-getting-output-from-executing-a-command-line-program

    val runtime = Runtime.getRuntime
    val command = cmd.mkString(" ")
    val process = runtime.exec(command)
    val tryRes = Using(new BufferedReader(new InputStreamReader(process.getInputStream))) { reader =>

      @tailrec def read(prevLinesRev: List[String]): List[String] = {
        val line = reader.readLine()
        if (line != null) {
          read(line :: prevLinesRev)
        } else {
          prevLinesRev.reverse
        }
      }

      reader.readLine() match {
        case "sat" => Sat(Z3OutputParser.parse(read(Nil)))
        case "unsat" => Unsat
        case "timeout" => Timeout(timeoutSec)
        case s => Error(s)
      }

    }
    tryRes match {
      case Failure(exception) => Error(exception.getMessage)
      case Success(result) => result
    }
  }

}
