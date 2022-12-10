package compiler.verification

import compiler.verification.Solver
import Solver.*
import smtlib.trees.Commands.{CheckSat, Script}
import smtlib.trees.{Commands, Terms}

import java.io.{BufferedReader, FileWriter, InputStreamReader}
import java.nio.file.Files
import scala.util.{Failure, Success, Try, Using}

object Z3Solver extends Solver {
  private val z3ExecName = "z3"
  private val tmpFilePath = java.nio.file.Paths.get("veriftmp", "z3input.smt")

  override def check(smtScript: Commands.Script): Result = {
    writeFile(smtScript).map { _ =>
      runCommand(z3ExecName, tmpFilePath.toString)
    }.getOrElse(Error("could not write file"))
  }

  override def check(smtScript: Commands.Script, timeoutSec: Int): Result = {
    writeFile(smtScript).map { _ =>
      val timeoutOption = s"-T:$timeoutSec"
      runCommand(z3ExecName, timeoutOption, tmpFilePath.toString)
    }.getOrElse(Error("could not write file"))
  }

  private def writeFile(script: Script): Try[Unit] = {
    Try {
      val parent = tmpFilePath.getParent.toFile
      if (!parent.isDirectory){
        parent.mkdir()
      }
      val file = tmpFilePath.toFile
      if (file.isFile){
        file.delete()
      }
      Files.createFile(tmpFilePath)
      val writer = new FileWriter(file)
      val printer = smtlib.printer.RecursivePrinter
      printer.printScript(script, writer)
      printer.printCommand(CheckSat(), writer)
      writer.flush()
    }
  }

  private def runCommand(cmd: String*): Result = {

    // adapted from https://stackoverflow.com/questions/5711084/java-runtime-getruntime-getting-output-from-executing-a-command-line-program

    val runtime = Runtime.getRuntime
    val command = cmd.mkString(" ")
    val process = runtime.exec(command)
    val tryRes = Using(new BufferedReader(new InputStreamReader(process.getInputStream))){ reader =>
      reader.readLine() match {
        case "sat" => Sat
        case "unsat" => Unsat
        case "timeout" => Timeout
        case s => Error(s)
      }
    }
    tryRes match {
      case Failure(exception) => Error(exception.getMessage)
      case Success(result) => result
    }
  }

}
