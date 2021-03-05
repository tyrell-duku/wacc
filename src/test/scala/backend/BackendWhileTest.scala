import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendWhileTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/while/")) {
    val name = f.getName()
    val (file, out, command) = createOutputFiles(f)
    test(s"While exit code test: $name") {
      assert(checkExitCode(file, command))
    }
    test(s"While expected test: $name") {
      assert(checkStdOut(file, out))
    }
  }
}
