import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._

class ExprTest extends AnyFunSuite {
  test("Successfully parses expr within parentheses") {
    assert(
      expr
        .runParser("((10))+(5)")
        .contains(
          Plus(Parens(Parens(IntLiter(10))), Parens(IntLiter(5)))
        )
    )
  }

  test("Successfully parses nested unary-oper") {
    assert(
      expr
        .runParser("ord(chr(10))")
        .contains(Ord(Parens(Chr(Parens(IntLiter(10))))))
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
              Mul(IntLiter(2), IntLiter(8)),
              Div(IntLiter(5), IntLiter(9))
            ),
            IntLiter(1)
          )
        )
    )
    assert(
      expr
        .runParser("100+8*5/9-1")
        .contains(
          Sub(
            Plus(
              IntLiter(100),
              Div(Mul(IntLiter(8), IntLiter(5)), IntLiter(9))
            ),
            IntLiter(1)
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
                IntLiter(100),
                Mul(IntLiter(8), IntLiter(5))
              ),
              IntLiter(100)
            ),
            IntLiter(1)
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
