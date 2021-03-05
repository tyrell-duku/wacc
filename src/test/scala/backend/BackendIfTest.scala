import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendIfTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/if/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    test(s"If exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    test(s"If expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
