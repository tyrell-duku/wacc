import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendArrayTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/array/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    ignore(s"Array exit code test for $name") {
      assert(checkExitCode(file, command))
    }
    ignore(s"Array expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
