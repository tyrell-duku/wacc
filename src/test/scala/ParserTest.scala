import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.Success

class UnaryOpTest extends AnyFunSuite {

  test("Successful parses not operator") {
    assertResult(true) { unaryOp.runParser("!").isSuccess }
  }

  test("Successful parses negation operator") {
    assertResult(true) { unaryOp.runParser("-").isSuccess }
  }

  test("Successful parses len operator") {
    assertResult(true) { unaryOp.runParser("len").isSuccess }
  }

  test("Successful parses ord operator") {
    assertResult(true) { unaryOp.runParser("ord").isSuccess }
  }

  test("Successful parses chr operator") {
    assertResult(true) { unaryOp.runParser("chr").isSuccess }
  }

  test("Successfully fails to parse random string '##!!!'") {
    assertResult(true) { unaryOp.runParser("##!!!").isFailure }
  }

  test("Successfully fails to parse random string 'l3n'") {
    assertResult(true) { unaryOp.runParser("l3n").isFailure }
  }

}
