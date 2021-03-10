import org.scalatest.funsuite.AnyFunSuite
import frontend.Parser._
import frontend.Rules._
import parsley.combinator.eof
import java.io.File

import parsley.Parsley
import frontend.Lexer._
import frontend.Semantics.SemanticChecker

class FrontendSemanticErrorTest extends AnyFunSuite {
  private def listAllFiles(dir: File): Array[File] = {
    val curFiles = dir.listFiles
    curFiles ++ curFiles.filter(_.isDirectory).flatMap(listAllFiles)
  }

  val programWhitespace: Parsley[Program] = lexer.whiteSpace *> program <* eof
  for (file <- listAllFiles(new File("wacc_examples/invalid/semanticErr"))) {
    if (file.isFile) {
      test("Semantically checks invalid file " + file.getName) {
        val checker = new SemanticChecker
        val (_, errors) =
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
        val (_, errors) =
          checker.progAnalysis(
            programWhitespace.parseFromFile(file).get
          )
        assert(errors.isEmpty)
      }
    }
  }
}
