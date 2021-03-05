import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._
import java.io.File

class BackendExpressionTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/expressions/")) {
    val name = f.getName()
    val (file, out, command) = createOutputFiles(f)
    test(s"Expression exit code test: $name") {
      assert(checkExitCode(file, out, command))
    }
    test(s"Expression expected test: $name") {
      assert(checkStdOut(file, out))
    }
  }
}
