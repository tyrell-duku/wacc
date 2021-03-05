import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendFunctionTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/function/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    test(s"Function exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    test(s"Function expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
