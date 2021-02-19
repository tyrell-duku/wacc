import org.scalatest.funsuite.AnyFunSuite
import Rules._
import LiterParser._
import Lexer._

class IdentifierTest extends AnyFunSuite {
  test("Successfully parses simple letter identifiers") {
    assert(identifier.runParser("x").contains(Ident("x",(1,1))))
    assert(identifier.runParser("abc").contains(Ident("abc",(1,1))))
    assert(identifier.runParser("xyz").contains(Ident("xyz",(1,1))))
    assert(identifier.runParser("ABC").contains(Ident("ABC",(1,1))))
  }

  test("Successfully parses simple alphanumeric identifiers") {
    assert(identifier.runParser("x1").contains(Ident("x1",(1,1))))
    assert(identifier.runParser("ABC20").contains(Ident("ABC20",(1,1))))
    assert(identifier.runParser("abc123").contains(Ident("abc123",(1,1))))
  }

  test("Successfully parses simple alphanumeric identifiers with underscore") {
    assert(identifier.runParser("_").contains(Ident("_",(1,1))))
    assert(
      identifier.runParser("temp_variable1").contains(Ident("temp_variable1",(1,1)))
    )
    assert(identifier.runParser("x1_y2").contains(Ident("x1_y2",(1,1))))
  }

  test("Successfully fails to parse identifiers with underscore") {
    assert(identifier.runParser("10x").isFailure)
    assert(identifier.runParser("!abc").isFailure)
  }
}
