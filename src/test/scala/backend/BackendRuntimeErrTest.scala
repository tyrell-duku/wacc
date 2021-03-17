import org.scalatest.funsuite.AnyFunSuite
import BackendTestHelper._
import java.io.File

class BackendRuntimeErrTest extends AnyFunSuite {
  for (f <- getFilesFrom("wacc_examples/valid/runtimeErr/")) {
    val name = f.getName()
    val (file, out, command) = createOutputFiles(f)
    ignore(s"Runtime error exit code test for $name") {
      assert(checkExitCode(file, command))
      out.delete()
    }
  }
}
