import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendScopeTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/scope/")) {
    val name = f.getName()
    val (file, out, command) = createOutputFiles(f)
    ignore(s"Scope exit code test: $name") {
      assert(checkExitCode(file, command))
    }
    ignore(s"Scope expected test: $name") {
      assert(checkStdOut(file, out))
    }
  }
}
