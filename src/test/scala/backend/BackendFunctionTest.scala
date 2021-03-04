// import org.scalatest.funsuite.AnyFunSuite
// import BackendTestHelper._

// class BackendFunctionTest extends AnyFunSuite {
//   for (f <- getFilesFrom("wacc_examples/valid/function/")) {
//     val name = f.getName
//     val (file, out, command) = createOutputFiles(f)
//     test(s"Basic exit code test for $name") {
//       assert(checkExitCode(file, out, command))
//     }
//     test(s"Basic expected test for $name") {
//       assert(checkStdOut(file, out))
//     }
//   }
// }
