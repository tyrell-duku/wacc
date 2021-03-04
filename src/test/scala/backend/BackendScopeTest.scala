// import org.scalatest.funsuite.AnyFunSuite
// import BackendTestHelper._

// class BackendScopeTest extends AnyFunSuite {
//   for (f <- getFilesFrom("wacc_examples/valid/scope/")) {
//     val name = f.getName()
//     val (file, out, command) = createOutputFiles(f)
//     test(s"Scope exit code test: $name") {
//       assert(checkExitCode(file, out, command))
//     }
//     test(s"Scope expected test: $name") {
//       assert(checkStdOut(file, out))
//     }
//   }
// }
