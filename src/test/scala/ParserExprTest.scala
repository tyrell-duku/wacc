import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._

class ExprTest extends AnyFunSuite {
  test("Successfully parses expr within parentheses") {
    assert(
      expr
        .runParser("((10))+(5)")
        .contains(
          Plus(Parens(Parens(IntLiter(None, 10))), Parens(IntLiter(None, 5)))
        )
    )
  }

  test("Successfully parses nested unary-oper") {
    assert(
      expr
        .runParser("ord(chr(10))")
        .contains(Ord(Parens(Chr(Parens(IntLiter(None, 10))))))
    )
  }

  test(
    "Successfully parses boolean unary-oper and binary-oper in correct order"
  ) {
    assert(
      expr
        .runParser("!(!true||false)")
        .contains(Not(Parens(Or(Not(BoolLiter(true)), BoolLiter(false)))))
    )
  }

  test("Successfully parses arithmetic binary-oper in correct order") {
    assert(
      expr
        .runParser("2*8+5/9-1")
        .contains(
          Sub(
            Plus(
              Mul(IntLiter(None, 2), IntLiter(None, 8)),
              Div(IntLiter(None, 5), IntLiter(None, 9))
            ),
            IntLiter(None, 1)
          )
        )
    )
    assert(
      expr
        .runParser("100+8*5/9-1")
        .contains(
          Sub(
            Plus(
              IntLiter(None, 100),
              Div(Mul(IntLiter(None, 8), IntLiter(None, 5)), IntLiter(None, 9))
            ),
            IntLiter(None, 1)
          )
        )
    )
    assert(
      expr
        .runParser("100+8*5+100-1")
        .contains(
          Sub(
            Plus(
              Plus(
                IntLiter(None, 100),
                Mul(IntLiter(None, 8), IntLiter(None, 5))
              ),
              IntLiter(None, 100)
            ),
            IntLiter(None, 1)
          )
        )
    )
  }

  test("Successfully fails parsing empty parenthesis ") {
    assert(expr.runParser("()").isFailure)
  }

  test("Successfully parses binary operators on logical atoms") {
    assert(
      expr
        .runParser("var1==(ord('c'))")
        .contains(
          Equal(Ident("var1"), Parens(Ord(Parens(CharLiter(NormalChar('c'))))))
        )
    )
  }
}
