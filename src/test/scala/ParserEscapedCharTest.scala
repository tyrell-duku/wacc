import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._

class EscapedCharTest extends AnyFunSuite {
  test("Successfully parses character 0") {
    assert(escapedChar.runParser("0").contains('0'))
  }

  test("Successfully parses character b") {
    assert(escapedChar.runParser("b").contains('b'))
  }

  test("Successfully parses character t") {
    assert(escapedChar.runParser("t").contains('t'))
  }

  test("Successfully parses character n") {
    assert(escapedChar.runParser("n").contains('n'))
  }

  test("Successfully parses character f") {
    assert(escapedChar.runParser("f").contains('f'))
  }

  test("Successfully parses character r") {
    assert(escapedChar.runParser("r").contains('r'))
  }

  test("Successfully parses character '\"' ") {
    assert(escapedChar.runParser("\"").contains('\"'))
  }

  test("Successfully parses character '") {
    assert(escapedChar.runParser("\'").contains('\''))
  }

  test("Successfully parses character '\' ") {
    assert(escapedChar.runParser("\\").contains('\\'))
  }
}
