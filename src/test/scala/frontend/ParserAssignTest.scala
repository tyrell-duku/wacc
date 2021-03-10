import org.scalatest.funsuite.AnyFunSuite
import frontend.Rules._
import frontend.StatParser._
import frontend.Lexer._
import parsley.combinator.eof

class FrontendParserAssignLHSTest extends AnyFunSuite {
  val assignLHSWhitespace = lexer.whiteSpace *> assignLHS <* eof

  test("Successfully parses ident") {
    assert(
      assignLHSWhitespace
        .runParser(" example")
        .contains(Ident("example", (1, 2)))
    )
  }

  test("Successfully parses an array-elem") {
    assert(
      assignLHSWhitespace
        .runParser("list[21][2]")
        .contains(
          ArrayElem(
            Ident("list", (1, 1)),
            List(IntLiter(21, (1, 6)), IntLiter(2, (1, 10))),
            (1, 1)
          )
        )
    )
  }

  test("Successfully parses a pair-elem") {
    assert(
      assignLHSWhitespace
        .runParser("fst 10")
        .contains(Fst(IntLiter(10, (1, 5)), (1, 1)))
    )
    assert(
      assignLHSWhitespace
        .runParser("snd x")
        .contains(Snd(Ident("x", (1, 5)), (1, 1)))
    )
  }
}

class FrontendParserAssignRHSTest extends AnyFunSuite {
  val assignRHSWhitespace = lexer.whiteSpace *> assignRHS <* eof

  test("Successfully parses call with no arguments") {
    assert(
      assignRHSWhitespace
        .runParser("call _p()")
        .contains(Call(Ident("_p", (1, 6)), None, (1, 1)))
    )
  }

  test("Successfully parses call with one argument") {
    assert(
      assignRHSWhitespace
        .runParser(" call   _p (65)")
        .contains(
          Call(
            Ident("_p", (1, 9)),
            Some(ArgList(List(IntLiter(65, (1, 13))))),
            (1, 2)
          )
        )
    )

  }

  test("Successfully parses call with multiple arguments") {
    assert(
      assignRHSWhitespace
        .runParser("call _var(q,r,s)")
        .contains(
          Call(
            Ident("_var", (1, 6)),
            Some(
              ArgList(
                List(
                  Ident("q", (1, 11)),
                  Ident("r", (1, 13)),
                  Ident("s", (1, 15))
                )
              )
            ),
            (1, 1)
          )
        )
    )
  }

  test("Successfully fails to parse embedded function calls") {
    assert(assignRHS.runParser("call _p(call q(call r()))").isFailure)
  }
}
