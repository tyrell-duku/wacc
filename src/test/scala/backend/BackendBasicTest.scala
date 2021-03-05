import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendBasicTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/basic/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    test(s"Basic exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    test(s"Basic expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
