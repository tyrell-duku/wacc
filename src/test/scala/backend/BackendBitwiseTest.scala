import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendBitwiseTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/bitwise/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    test(s"Bitwise exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    test(s"Bitwise expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
