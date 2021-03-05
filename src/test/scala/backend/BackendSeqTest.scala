import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._

class BackendSeqTest extends AnyFunSuite {

  for (f <- getFilesFrom("wacc_examples/valid/sequence/")) {
    val name = f.getName
    val (file, out, command) = createOutputFiles(f)
    test(s"Sequence exit code test for $name") {
      assert(checkExitCode(file, out, command))
    }
    test(s"Sequence expected test for $name") {
      assert(checkStdOut(file, out))
    }
  }
}
