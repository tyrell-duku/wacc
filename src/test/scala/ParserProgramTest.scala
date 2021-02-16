import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof
import java.io.File
import parsley.Parsley
import Lexer._

class ProgramTest extends AnyFunSuite {
  val programWhitespace: Parsley[Program] = lexer.whiteSpace *> program <* eof

  private def listAllFiles(dir: File): Array[File] = {
    val curFiles = dir.listFiles
    curFiles ++ curFiles.filter(_.isDirectory).flatMap(listAllFiles)
  }

  for (file <- listAllFiles(new File("wacc_examples/valid"))) {
    if (file.isFile) {
      test("Successfully parses file " + file.getName) {
        assert(programWhitespace.parseFromFile(file).isSuccess)
      }
    }
  }

  for (file <- listAllFiles(new File("wacc_examples/invalid/syntaxErr"))) {
    if (file.isFile) {
      test("Successfully fails to parse file " + file.getName) {
        if (programWhitespace.parseFromFile(file).isSuccess) {
          assert(programWhitespace.parseFromFile(file).contains())
        }
      }
    }
  }
}
