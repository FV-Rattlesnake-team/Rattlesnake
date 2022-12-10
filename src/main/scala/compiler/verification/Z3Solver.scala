package compiler.verification

import compiler.verification.Solver
import Solver.*
import smtlib.trees.Commands.{CheckSat, Script}
import smtlib.trees.{Commands, Terms}

import java.io.{BufferedReader, FileWriter, InputStreamReader}
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Success, Try, Using}

final class Z3Solver(outputDir: java.nio.file.Path) extends Solver {
  private val z3ExecName = "z3"

  {
    val dir = outputDir.toFile
    if (dir.exists()){
      clearDir()
    } else {
      dir.createNewFile()
    }
  }

  private def nextFilepath(idx: Int) = {
    outputDir.resolve(s"z3input_$idx.smt")
  }


  override def check(smtScript: Commands.Script, comments: List[String], idx: Int): Result = {
    val filepath = nextFilepath(idx)
    writeFile(smtScript, filepath, comments).map { _ =>
      runCommand(z3ExecName, filepath.toString)
    }.getOrElse(Error("could not write file"))
  }

  override def check(smtScript: Commands.Script, timeoutSec: Int, comments: List[String], idx: Int): Result = {
    val filepath = nextFilepath(idx)
    writeFile(smtScript, filepath, comments).map { _ =>
      val timeoutOption = s"-T:$timeoutSec"
      runCommand(z3ExecName, timeoutOption, filepath.toString)
    }.getOrElse(Error("could not write file"))
  }

  private def clearDir(): Unit = {
    for file <- outputDir.toFile.listFiles() do {
      file.delete()
    }
  }

  private def writeFile(script: Script, tmpFilePath: java.nio.file.Path, comments: List[String]): Try[Unit] = {
    val file = tmpFilePath.toFile
    if (!file.exists()) {
      Files.createFile(tmpFilePath)
    }
    Using(new FileWriter(file)) { writer =>
      writeComments(comments, writer)
      val printer = smtlib.printer.RecursivePrinter
      printer.printScript(script, writer)
      printer.printCommand(CheckSat(), writer)
      writer.flush()
    }
  }

  private def writeComments(lines: List[String], writer: FileWriter): Unit = {
    for line <- lines do {
      writer.write(s"; $line\n")
    }
  }

  private def runCommand(cmd: String*): Result = {

    // adapted from https://stackoverflow.com/questions/5711084/java-runtime-getruntime-getting-output-from-executing-a-command-line-program

    val runtime = Runtime.getRuntime
    val command = cmd.mkString(" ")
    val process = runtime.exec(command)
    val tryRes = Using(new BufferedReader(new InputStreamReader(process.getInputStream))) { reader =>
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
