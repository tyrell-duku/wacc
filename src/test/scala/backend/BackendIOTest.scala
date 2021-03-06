import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendIOTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/IO/")) {
    val name = f.getName()
    val (file, out, command) = createOutputFiles(f)
    test(s"IO exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    test(s"IO expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
