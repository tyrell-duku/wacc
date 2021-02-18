import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof
import java.io.File
import parsley.Parsley
import Lexer._

class SemanticErrorTest extends AnyFunSuite {
  private def listAllFiles(dir: File): Array[File] = {
    val curFiles = dir.listFiles
    curFiles ++ curFiles.filter(_.isDirectory).flatMap(listAllFiles)
  }

  val programWhitespace: Parsley[Program] = lexer.whiteSpace *> program <* eof
  for (file <- listAllFiles(new File("wacc_examples/invalid/semanticErr"))) {
    if (file.isFile) {
      test("Semantically checks invalid file " + file.getName) {
        val checker = new SemanticChecker
        val errors =
          checker.progAnalysis(
            programWhitespace.parseFromFile(file).get
          )
        assert(errors.nonEmpty)
      }
    }
  }

  for (file <- listAllFiles(new File("wacc_examples/valid"))) {
    if (file.isFile) {
      test("Semantically checks valid file " + file.getName) {
        val checker = new SemanticChecker
        val errors =
          checker.progAnalysis(
            programWhitespace.parseFromFile(file).get
          )
        assert(errors.isEmpty)
      }
    }
  }
}
