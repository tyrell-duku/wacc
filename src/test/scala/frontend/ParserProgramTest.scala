import org.scalatest.funsuite.AnyFunSuite
import frontend.Parser._
import frontend.Rules._
import parsley.combinator.eof
import java.io.File
import parsley.Parsley
import frontend.Lexer._

class FrontendParserProgramTest extends AnyFunSuite {
  val programWhitespace: Parsley[Program] = lexer.whiteSpace *> program <* eof
  private val skip =
    Array.empty // listAllFiles(new File("wacc_examples/valid/bitwise"))

  private def testFile(
      testFunc: ((String, List[org.scalatest.Tag]) => (=> Any) => Unit),
      testName: String,
      file: File,
      checkFunc: (parsley.Result[frontend.Rules.Program] => Boolean)
  ) = {
    if (file.isFile()) {
      testFunc(testName + file.getName, List.empty) {
        assert(checkFunc(programWhitespace.parseFromFile(file)))
      }
    }
  }

  private def listAllFiles(dir: File): Array[File] = {
    val curFiles = dir.listFiles
    curFiles ++ curFiles.filter(_.isDirectory).flatMap(listAllFiles)
  }

  for (file <- listAllFiles(new File("wacc_examples/valid"))) {
    if (skip.contains(file)) {
      testFile(ignore, "Successfully parses file ", file, (x => x.isSuccess))
    } else {
      testFile(test, "Successfully parses file ", file, (x => x.isSuccess))
    }
  }

  for (file <- listAllFiles(new File("wacc_examples/invalid/syntaxErr"))) {
    if (skip.contains(file)) {
      testFile(
        ignore,
        "Successfully fails to parse file ",
        file,
        (x => x.isFailure)
      )
    } else {
      testFile(
        test,
        "Successfully fails to parse file ",
        file,
        (x => x.isFailure)
      )
    }
  }
}
