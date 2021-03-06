import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendFullHeapTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/fullHeap/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    test(s"Full heap exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    test(s"Full heap expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
