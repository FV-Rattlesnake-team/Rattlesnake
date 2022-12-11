package testutil

import java.io.File
import java.nio.file.Files

object TestsIO {

  def deleteRecursively(file: File): Unit = {
    val subFiles = file.listFiles()
    if (subFiles != null) {
      for file <- subFiles do {
        deleteRecursively(file)
      }
    }
    Files.delete(file.toPath)
  }

}
