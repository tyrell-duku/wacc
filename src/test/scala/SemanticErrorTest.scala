import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof
import java.io.File
import parsley.Parsley

class SemanticErrorTest extends AnyFunSuite {
  private def listAllFiles(dir: File): Array[File] = {
    val curFiles = dir.listFiles
    curFiles ++ curFiles.filter(_.isDirectory).flatMap(listAllFiles)
  }

  val programWhitespace: Parsley[Program] = lexer.whiteSpace *> program <* eof
  for (file <- listAllFiles(new File("wacc_examples/invalid/semanticErr"))) {
    if (file.isFile) {
      test("Successfully fails to parse " + file.getName) {
        var checker = new SemanticChecker
        var errors =
          checker.progAnalysis(
            programWhitespace.parseFromFile(file).get
          )
        println(file.getName)
        errors.foreach((s: SemanticError) => println(s))
        println()
      }

    }
  }
}
