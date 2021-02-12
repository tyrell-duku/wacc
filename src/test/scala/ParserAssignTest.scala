import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import LiterParser._
import ExprParser._
import StatParser._
import Lexer._
import parsley.combinator.eof

class AssignLHSTest extends AnyFunSuite {
  val assignLHSWhitespace = lexer.whiteSpace *> assignLHS <* eof

  test("Successfully parses ident") {
    assert(assignLHSWhitespace.runParser(" example").contains(Ident("example")))
  }

  test("Successfully parses an array-elem") {
    assert(
      assignLHSWhitespace
        .runParser("list[21][2]")
        .contains(ArrayElem(Ident("list"), List(IntLiter(21), IntLiter(2))))
    )
  }

  test("Successfully parses a pair-elem") {
    assert(assignLHSWhitespace.runParser("fst 10").contains(Fst(IntLiter(10))))
    assert(assignLHSWhitespace.runParser("snd x").contains(Snd(Ident("x"))))
  }
}

class AssignRHSTest extends AnyFunSuite {
  val assignRHSWhitespace = lexer.whiteSpace *> assignRHS <* eof

  test("Successfully parses call with no arguments") {
    assert(assignRHS.runParser("call_p()").contains(Call(Ident("_p"), None)))
    assert(
      assignRHSWhitespace
        .runParser("call _p()")
        .contains(Call(Ident("_p"), None))
    )
  }

  test("Successfully parses call with one argument") {
    assert(
      assignRHS
        .runParser("call_var(q)")
        .contains(Call(Ident("_var"), Some(ArgList(List(Ident("q"))))))
    )
    assert(
      assignRHSWhitespace
        .runParser(" call   _p (65)")
        .contains(Call(Ident("_p"), Some(ArgList(List(IntLiter(65))))))
    )

  }

  test("Successfully parses call with multiple arguments") {
    assert(
      assignRHS
        .runParser("call_var(q,r,s)")
        .contains(
          Call(
            Ident("_var"),
            Some(ArgList(List(Ident("q"), Ident("r"), Ident("s"))))
          )
        )
    )
  }

  test("Successfully fails to parse embedded function calls") {
    assert(assignRHS.runParser("call_p(call q(call r()))").isFailure)
  }
}
