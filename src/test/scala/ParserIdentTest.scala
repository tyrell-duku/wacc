import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import LiterParser._
import StatParser._
import ExprParser._
import Lexer._

class IdentifierTest extends AnyFunSuite {
  test("Successfully parses simple letter identifiers") {
    assert(identifier.runParser("x").contains(Ident("x")))
    assert(identifier.runParser("abc").contains(Ident("abc")))
    assert(identifier.runParser("xyz").contains(Ident("xyz")))
    assert(identifier.runParser("ABC").contains(Ident("ABC")))
  }

  test("Successfully parses simple alphanumeric identifiers") {
    assert(identifier.runParser("x1").contains(Ident("x1")))
    assert(identifier.runParser("ABC20").contains(Ident("ABC20")))
    assert(identifier.runParser("abc123").contains(Ident("abc123")))
  }

  test("Successfully parses simple alphanumeric identifiers with underscore") {
    assert(identifier.runParser("_").contains(Ident("_")))
    assert(
      identifier.runParser("temp_variable1").contains(Ident("temp_variable1"))
    )
    assert(identifier.runParser("x1_y2").contains(Ident("x1_y2")))
  }

  test("Successfully fails to parse identifiers with underscore") {
    assert(identifier.runParser("10x").isFailure)
    assert(identifier.runParser("!abc").isFailure)
  }
}
