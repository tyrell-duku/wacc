import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import LiterParser._
import StatParser._
import ExprParser._
import Lexer._
class CharacterTest extends AnyFunSuite {
  test("Successfully parses escaped b character") {
    assert(character.runParser("\\b").contains(Escape('b')))
  }

  test("Successfully fails to parse '\\' character") {
    assert(character.runParser("\\").isFailure)
  }

  test("Successfully parses the character 'a'") {
    assert(character.runParser("a").contains(NormalChar('a')))
  }
}
