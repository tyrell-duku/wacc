import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendVariablesTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/variables/")) {
    val name = f.getName()
    val (file, out, command) = createOutputFiles(f)
    test(s"Variables exit code test: $name") {
      assert(checkExitCode(file, out, command))
    }
    test(s"Variables expected test: $name") {
      assert(checkStdOut(file, out))
    }
  }
}
