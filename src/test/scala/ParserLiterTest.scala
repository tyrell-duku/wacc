import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof

class IntLiterTest extends AnyFunSuite {
  test("Successfully parses digit without sign") {
    assert(
      intLiter.runParser("100").contains(IntLiter(None, 100))
    )
    assert(intLiter.runParser("1").contains(IntLiter(None, 1)))
  }

  test("Successfully parses digit with sign") {
    assert(
      intLiter.runParser("+100").contains(IntLiter(Some(Pos), 100))
    )
    assert(
      intLiter.runParser("-100").contains(IntLiter(Some(Neg), 100))
    )
  }

  test("Successfully fails to parse sign without digit") {
    assert(intLiter.runParser("+").isFailure)
    assert(intLiter.runParser("-").isFailure)

  }

  test("Successfully fails to parse number with decimal part") {
    assert(intLiter.runParser("+100.5").contains(IntLiter(Some(Pos), 100)))
    assert(intLiter.runParser("-0.5").contains(IntLiter(Some(Neg), 0)))
  }
}

class BoolLiteralTest extends AnyFunSuite {
  test("Successfully parses true") {
    assert(boolLiteral.runParser("true").contains(BoolLiter(true)))
  }

  test("Successfully parses false") {
    assert(boolLiteral.runParser("false").contains(BoolLiter(false)))
  }
}

class CharLitTest extends AnyFunSuite {
  test("Successfully parses escaped b char literal") {
    assert(charLiteral.runParser("'\\b'").contains(CharLiter(Escape('b'))))
  }

  test("Successfully fails to parse '\\' char literal") {
    assert(charLiteral.runParser("'\\'").isFailure)
  }

  test("Successfully parses the char literal 'a'") {
    assert(charLiteral.runParser("'a'").contains(CharLiter(NormalChar('a'))))
  }
}

class StringLitTest extends AnyFunSuite {
  test("Successfully parses string literal \"hello world\"") {
    assert(strLiteral.runParser("\"hello world\"").isSuccess)
  }

  test("Successfully fails to parse \"Joe's\" string literal") {
    assert(strLiteral.runParser("\"Joe's\"").isFailure)
  }

  test("Successfully parses string literal \"\\thello world\"") {
    assert(strLiteral.runParser("\"\\thello world\"").isSuccess)
  }
}

class ArrayLiterTest extends AnyFunSuite {
  test("Successfully parses double array literal") {
    assert(
      arrayLiter
        .runParser("[10][2]")
        .contains(
          ArrayLiter(Some(List(IntLiter(None, 10))))
        )
    )
  }

  test("Successfully parses an empty array literal") {
    assert(arrayLiter.runParser("[]").contains(ArrayLiter(None)))
  }

  test("Successfully parses single element array literal") {
    assert(
      arrayLiter
        .runParser("[10]")
        .contains(ArrayLiter(Some(List(IntLiter(None, 10)))))
    )

  }
  test("Successfully parses array elem within array literal") {
    assert(
      arrayLiter
        .runParser("[var[10]]")
        .contains(
          ArrayLiter(
            Some(List(ArrayElem(Ident("var"), List(IntLiter(None, 10)))))
          )
        )
    )
  }

  test("Fails to parse array literal within itself") {
    assert(arrayLiter.runParser("[[10,9,8]]").isFailure)
  }

  test("Successfully parses multiple expression array literal") {
    assert(
      arrayLiter
        .runParser("[10,9,8]")
        .contains(
          ArrayLiter(
            Some(List(IntLiter(None, 10), IntLiter(None, 9), IntLiter(None, 8)))
          )
        )
    )
    assert(
      arrayLiter
        .runParser("['a','b','c']")
        .contains(
          ArrayLiter(
            Some(
              List(
                CharLiter(NormalChar('a')),
                CharLiter(NormalChar('b')),
                CharLiter(NormalChar('c'))
              )
            )
          )
        )
    )
  }

  val arrayLiterWhitespace = lexer.whiteSpace *> arrayLiter <* eof

  test("Successfully parses int array literal, with whitespace") {
    assert(
      arrayLiterWhitespace
        .runParser("[ 12, 67]")
        .contains(
          ArrayLiter(Some(List(IntLiter(None, 12), IntLiter(None, 67))))
        )
    )
  }

  test("Successfully parses char array literal, with whitespace") {
    assert(
      arrayLiterWhitespace
        .runParser("['a', 'b', 'c']")
        .contains(
          ArrayLiter(
            Some(
              List(
                CharLiter(NormalChar('a')),
                CharLiter(NormalChar('b')),
                CharLiter(NormalChar('c'))
              )
            )
          )
        )
    )
  }
}

// TODO: pairLiterTest
