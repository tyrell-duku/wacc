import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._
import java.io.File

class BackendPairsTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/pairs/")) {
    val name = f.getName()
    val (file, out, command) = createOutputFiles(f)
    ignore(s"Pairs exit code test: $name") {
      assert(checkExitCode(file, command))
    }
    ignore(s"Pairs expected test: $name") {
      assert(checkStdOut(file, out))
    }
  }
}
