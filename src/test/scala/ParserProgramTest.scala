import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof
import java.io.File
import parsley.Parsley

class ProgramTest extends AnyFunSuite {
  val programWhitespace: Parsley[Program] = lexer.whiteSpace *> program <* eof

   private def listAllFiles(dir: File): Array[File] = {
     val curFiles = dir.listFiles
     curFiles ++ curFiles.filter(_.isDirectory).flatMap(listAllFiles)
   }

   for (file <- listAllFiles(new File("wacc_examples/valid"))) {
     if (file.isFile) {
       test("Successfully parses " + file.getName) {
         assert(programWhitespace.parseFromFile(file).isSuccess)
       }
     }
   }
}
