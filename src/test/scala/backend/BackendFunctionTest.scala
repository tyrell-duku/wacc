import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendFunctionTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/function/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    ignore(s"Function exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    ignore(s"Function expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
