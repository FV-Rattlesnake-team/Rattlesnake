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
  @Test def loopFailTest1(): Unit = runVerifTest("loopFail1", false)
  @Test def loopFailTest2(): Unit = runVerifTest("loopFail2", false)
  @Test def verifValTest(): Unit = runVerifTest("verifval", true)

  private def runVerifTest(filename: String, expectedRes: Boolean, timeoutSec: Int = 2): Unit = {
    val pipeline = TasksPipelines.verifier(Paths.get(outputDirPath), timeoutSec, logger = _ => ())
    val path = s"$resDirPath/$filename.${FileExtensions.rattlesnake}"
    val actualRes = pipeline.apply(List(SourceFile(path)))
    assertEquals(expectedRes, actualRes)
  }

}
