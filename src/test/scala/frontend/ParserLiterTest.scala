import org.scalatest.funsuite.AnyFunSuite
import frontend.Rules._
import parsley.combinator.eof
import frontend.LiterParser._
import frontend.ExprParser._
import frontend.Lexer._

class ParserIntLiterTest extends AnyFunSuite {
  test("Successfully parses digit without sign") {
    assert(
      intLiter.runParser("100").contains(IntLiter(100, (1, 1)))
    )
    assert(intLiter.runParser("1").contains(IntLiter(1, (1, 1))))
  }

  test("Successfully parses digit with sign") {
    assert(
      intLiter.runParser("+100").contains(IntLiter(100, (1, 1)))
    )
    assert(
      intLiter.runParser("-100").contains(IntLiter(-100, (1, 1)))
    )
  }

  test("Successfully fails to parse sign without digit") {
    assert(intLiter.runParser("+").isFailure)
    assert(intLiter.runParser("-").isFailure)

  }

  test("Successfully fails to parse number with decimal part") {
    assert(intLiter.runParser("+100.5").contains(IntLiter(100, (1, 1))))
    assert(intLiter.runParser("-0.5").contains(IntLiter(-0, (1, 1))))
  }
}

class ParserBoolLiteralTest extends AnyFunSuite {
  test("Successfully parses true") {
    assert(boolLiteral.runParser("true").contains(BoolLiter(true, (1, 1))))
  }

  test("Successfully parses false") {
    assert(boolLiteral.runParser("false").contains(BoolLiter(false, (1, 1))))
  }
}

class ParserCharLitTest extends AnyFunSuite {
  test("Successfully parses escaped b char literal") {
    assert(
      charLiteral.runParser("'\\b'").contains(CharLiter(Escape('b'), (1, 1)))
    )
  }

  test("Successfully fails to parse '\\' char literal") {
    assert(charLiteral.runParser("'\\'").isFailure)
  }

  test("Successfully parses the char literal 'a'") {
    assert(
      charLiteral.runParser("'a'").contains(CharLiter(NormalChar('a'), (1, 1)))
    )
  }
}

class ParserStringLitTest extends AnyFunSuite {
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

class ParserArrayLiterTest extends AnyFunSuite {
  val arrayLiterWhitespace = lexer.whiteSpace *> arrayLiter <* eof

  test("Successfully parses double array literal") {
    assert(
      arrayLiter
        .runParser("[10][2]")
        .contains(
          ArrayLiter(Some(List(IntLiter(10, (1, 2)))), (1, 1))
        )
    )
  }

  test("Successfully parses an empty array literal") {
    assert(arrayLiter.runParser("[]").contains(ArrayLiter(None, (1, 1))))
  }

  test("Successfully parses single element array literal") {
    assert(
      arrayLiter
        .runParser("[10]")
        .contains(ArrayLiter(Some(List(IntLiter(10, (1, 2)))), (1, 1)))
    )

  }
  test("Successfully parses array elem within array literal") {
    assert(
      arrayLiter
        .runParser("[var[10]]")
        .contains(
          ArrayLiter(
            Some(
              List(
                ArrayElem(
                  Ident("var", (1, 2)),
                  List(IntLiter(10, (1, 6))),
                  (1, 2)
                )
              )
            ),
            (1, 1)
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
            Some(
              List(
                IntLiter(10, (1, 2)),
                IntLiter(9, (1, 5)),
                IntLiter(8, (1, 7))
              )
            ),
            (1, 1)
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
                CharLiter(NormalChar('a'), (1, 2)),
                CharLiter(NormalChar('b'), (1, 6)),
                CharLiter(NormalChar('c'), (1, 10))
              )
            ),
            (1, 1)
          )
        )
    )
  }

  test("Successfully parses int array literal, with whitespace") {
    assert(
      arrayLiterWhitespace
        .runParser("[ 12, 67]")
        .contains(
          ArrayLiter(
            Some(List(IntLiter(12, (1, 3)), IntLiter(67, (1, 7)))),
            (1, 1)
          )
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
                CharLiter(NormalChar('a'), (1, 2)),
                CharLiter(NormalChar('b'), (1, 7)),
                CharLiter(NormalChar('c'), (1, 12))
              )
            ),
            (1, 1)
          )
        )
    )
  }
}

class ParserPairLiterTest extends AnyFunSuite {
  test("Successfully parses pair-liter") {
    assert(pairLiteral.runParser("null").contains(PairLiter((1, 1))))
  }
}
