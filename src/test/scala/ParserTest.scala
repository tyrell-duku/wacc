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

class BaseTypeTest extends AnyFunSuite {
  test("Successfully parses int type") {
    assertResult(true) { baseType.runParser("int").isSuccess }
  }

  test("Successfully parses bool type") {
    assertResult(true) { baseType.runParser("bool").isSuccess }
  }

  test("Successfully parses char type") {
    assertResult(true) { baseType.runParser("char").isSuccess }
  }

  test("Successfully parses string type") {
    assertResult(true) { baseType.runParser("string").isSuccess }
  }

  test("Successfully fails to parse string 'float'") {
    assertResult(true) { unaryOp.runParser("float").isFailure }
  }

}
