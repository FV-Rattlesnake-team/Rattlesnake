package compiler

import compiler.io.SourceFile
import compiler.verification.Solver
import org.junit.Assert.assertEquals
import org.junit.{After, Before, Test}
import testutil.TestsIO

import java.io.File
import java.nio.file.Paths

class VerifierTests {
  private val outputDirPath = "verifierTestsTmp"
  private val resDirPath = "src/test/res/verif"

  @After def deleteTmpDir(): Unit = {
    TestsIO.deleteRecursively(new File(outputDirPath))
  }

  @Test def answer42Test(): Unit = runVerifTest("answer42", true)
  @Test def reluTest(): Unit = runVerifTest("relu", true)
  @Test def reluFailTest(): Unit = runVerifTest("reluFail", false)
  @Test def squareTest(): Unit = runVerifTest("square", true)
  @Test def verifLoopTest(): Unit = runVerifTest("verifloop", true)
  @Test def loopFailTest(): Unit = runVerifTest("loopFail", false)
  @Test def verifValTest(): Unit = runVerifTest("verifval", true)

  private def runVerifTest(filename: String, expectedRes: Boolean, timeout: Int = 2): Unit = {
    val pipeline = TasksPipelines.verifier(Paths.get(outputDirPath), timeoutSecOpt = Some(timeout), logger = println)
    val path = s"$resDirPath/$filename.${FileExtensions.rattlesnake}"
    val actualRes = pipeline.apply(List(SourceFile(path)))
    assertEquals(expectedRes, actualRes)
  }

}
