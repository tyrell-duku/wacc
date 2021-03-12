import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendBitwiseTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/bitwise/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    ignore(s"Bitwise exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    ignore(s"Bitwise expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
