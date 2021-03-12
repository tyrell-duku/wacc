import org.scalatest.funsuite.AnyFunSuite
import frontend.Parser._
import frontend.Rules._
import parsley.combinator.eof
import java.io.File
import parsley.Parsley
import frontend.Lexer._
import frontend.Semantics.SemanticChecker
import scala.collection.mutable.ListBuffer
import frontend.Semantics.SemanticError
import Array.concat

class FrontendSemanticErrorTest extends AnyFunSuite {
  val programWhitespace: Parsley[Program] = lexer.whiteSpace *> program <* eof
  private val skip = listAllFiles(new File("wacc_examples/valid/fullHeap"))

  private def testFile(
      testFunc: ((String, List[org.scalatest.Tag]) => (=> Any) => Unit),
      testName: String,
      file: File,
      checkFunc: (ListBuffer[SemanticError] => Boolean)
  ) = {
    if (file.isFile()) {
      testFunc(testName + file.getName, List.empty) {
        val checker = new SemanticChecker
        val (_, errors) =
          checker.progAnalysis(
            programWhitespace.parseFromFile(file).get
          )
        assert(checkFunc(errors))
      }
    }
  }

  private def listAllFiles(dir: File): Array[File] = {
    val curFiles = dir.listFiles
    curFiles ++ curFiles.filter(_.isDirectory).flatMap(listAllFiles)
  }

  for (file <- listAllFiles(new File("wacc_examples/invalid/semanticErr"))) {
    if (skip.contains(file)) {
      testFile(
        ignore,
        "Semantically checks invalid file ",
        file,
        (x => x.nonEmpty)
      )
    } else {
      testFile(
        test,
        "Semantically checks invalid file ",
        file,
        (x => x.nonEmpty)
      )
    }
  }

  for (file <- listAllFiles(new File("wacc_examples/valid"))) {
    if (skip.contains(file)) {
      testFile(
        ignore,
        "Semantically checks valid file ",
        file,
        (x => x.isEmpty)
      )
    } else {
      testFile(test, "Semantically checks valid file ", file, (x => x.isEmpty))
    }
  }
}
