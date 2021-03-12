import org.scalatest.funsuite.AnyFunSuite
import frontend.Rules._
import frontend.ExprParser._
import frontend.Lexer._

class FrontendParserExprTest extends AnyFunSuite {
  test("Successfully parses expr within parentheses") {
    assert(
      expr
        .runParser("((10))+(5)")
        .contains(
          Plus(IntLiter(10, (1, 3)), IntLiter(5, (1, 9)), (1, 7))
        )
    )
    assert(
      expr
        .runParser("((26))+(sizeof(int[]))")
        .contains(
          Plus(IntLiter(26, (1, 3)), SizeOf(ArrayT(IntT), (1, 9)), (1, 7))
        )
    )
  }

  test("Successfully parses nested unary-oper") {
    assert(
      expr
        .runParser("ord(chr(10))")
        .contains(Ord(Chr(IntLiter(10, (1, 9)), (1, 5)), (1, 1)))
    )
  }

  test(
    "Successfully parses boolean unary-oper and binary-oper in correct order"
  ) {
    assert(
      expr
        .runParser("!(!true||false)")
        .contains(
          Not(
            Or(
              Not(BoolLiter(true, (1, 4)), (1, 3)),
              BoolLiter(false, (1, 10)),
              (1, 8)
            ),
            (1, 1)
          )
        )
    )
  }

  test("Successfully parses arithmetic binary-oper in correct order") {
    assert(
      expr
        .runParser("2*8+5/9-1")
        .contains(
          Sub(
            Plus(
              Mul(IntLiter(2, (1, 1)), IntLiter(8, (1, 3)), (1, 2)),
              Div(IntLiter(5, (1, 5)), IntLiter(9, (1, 7)), (1, 6)),
              (1, 4)
            ),
            IntLiter(1, (1, 9)),
            (1, 8)
          )
        )
    )
    assert(
      expr
        .runParser("100+8*5/9-1")
        .contains(
          Sub(
            Plus(
              IntLiter(100, (1, 1)),
              Div(
                Mul(IntLiter(8, (1, 5)), IntLiter(5, (1, 7)), (1, 6)),
                IntLiter(9, (1, 9)),
                (1, 8)
              ),
              (1, 4)
            ),
            IntLiter(1, (1, 11)),
            (1, 10)
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
                IntLiter(100, (1, 1)),
                Mul(IntLiter(8, (1, 5)), IntLiter(5, (1, 7)), (1, 6)),
                (1, 4)
              ),
              IntLiter(100, (1, 9)),
              (1, 8)
            ),
            IntLiter(1, (1, 13)),
            (1, 12)
          )
        )
    )
    assert(
      expr
        .runParser("2*8+sizeof(int)")
        .contains(
          Plus(
            Mul(IntLiter(2, (1, 1)), IntLiter(8, (1, 3)), (1, 2)),
            SizeOf(IntT, (1, 5)),
            (1, 4)
          )
        )
    )
  }

  test("Successfully fails parsing empty parenthesis") {
    assert(expr.runParser("()").isFailure)
  }

  test("Successfully parses binary operators on logical atoms") {
    assert(
      expr
        .runParser("var1==(ord('c'))")
        .contains(
          Equal(
            Ident("var1", (1, 1)),
            Ord(CharLiter(NormalChar('c'), (1, 12)), (1, 8)),
            (1, 5)
          )
        )
    )
  }

  // TODO: positions
  ignore("Successfully parses bitwise operators with numbers") {
    assert(
      expr
        .runParser("5&10")
        .contains(BitWiseAnd(IntLiter(5, (1, 1)), IntLiter(10, (1, 1)), (1, 1)))
    )
    assert(
      expr
        .runParser("5 |  10")
        .contains(BitWiseOr(IntLiter(5, (1, 1)), IntLiter(10, (1, 1)), (1, 1)))
    )
    assert(
      expr
        .runParser("5 ^ 10")
        .contains(BitWiseXor(IntLiter(5, (1, 1)), IntLiter(10, (1, 1)), (1, 1)))
    )
    assert(
      expr
        .runParser("5 << 2")
        .contains(
          LogicalLeftShift(IntLiter(5, (1, 1)), IntLiter(2, (1, 1)), (1, 1))
        )
    )
    assert(
      expr
        .runParser("20 >> 2")
        .contains(
          LogicalRightShift(IntLiter(20, (1, 1)), IntLiter(2, (1, 1)), (1, 1))
        )
    )
  }

  // TODO: positions
  ignore("Successfully parses bitwise operators in correct order") {
    assert(
      expr
        .runParser("5|10 & 100")
        .contains(
          BitWiseOr(
            IntLiter(5, (1, 1)),
            BitWiseAnd(IntLiter(10, (1, 1)), IntLiter(100, (1, 1)), (1, 1)),
            (1, 1)
          )
        )
    )
    assert(
      expr
        .runParser("5 |  10 ^ 2")
        .contains(
          BitWiseOr(
            IntLiter(5, (1, 1)),
            BitWiseXor(IntLiter(10, (1, 1)), IntLiter(2, (1, 1)), (1, 1)),
            (1, 1)
          )
        )
    )
    assert(
      expr
        .runParser("4 ^ 5 & 10")
        .contains(
          BitWiseXor(
            IntLiter(4, (1, 1)),
            BitWiseAnd(IntLiter(5, (1, 1)), IntLiter(10, (1, 1)), (1, 1)),
            (1, 1)
          )
        )
    )
    assert(
      expr
        .runParser("5 << 2 & 3")
        .contains(
          BitWiseAnd(
            IntLiter(5, (1, 1)),
            LogicalLeftShift(IntLiter(2, (1, 1)), IntLiter(3, (1, 1)), (1, 1)),
            (1, 1)
          )
        )
    )
    assert(
      expr
        .runParser("20 >> 2 | 10")
        .contains(
          BitWiseOr(
            IntLiter(20, (1, 1)),
            LogicalRightShift(
              IntLiter(2, (1, 1)),
              IntLiter(10, (1, 1)),
              (1, 1)
            ),
            (1, 1)
          )
        )
    )
  }

  // TODO: positions
  ignore("Successfully parses bitwise operators with other operators") {
    assert(
      expr
        .runParser("20 >> 2 > 8")
        .contains(
          GT(
            LogicalRightShift(
              IntLiter(20, (1, 1)),
              IntLiter(2, (1, 1)),
              (1, 1)
            ),
            IntLiter(8, (1, 1)),
            (1, 1)
          )
        )
    )
    assert(
      expr
        .runParser("20 << 2 < 8")
        .contains(
          LT(
            LogicalLeftShift(
              IntLiter(20, (1, 1)),
              IntLiter(2, (1, 1)),
              (1, 1)
            ),
            IntLiter(8, (1, 1)),
            (1, 1)
          )
        )
    )
    assert(
      expr
        .runParser("20 > 8 >> 2")
        .contains(
          GT(
            IntLiter(20, (1, 1)),
            LogicalRightShift(
              IntLiter(8, (1, 1)),
              IntLiter(2, (1, 1)),
              (1, 1)
            ),
            (1, 1)
          )
        )
    )
    assert(
      expr
        .runParser("20 < 2 << 3")
        .contains(
          LT(
            IntLiter(20, (1, 1)),
            LogicalLeftShift(
              IntLiter(2, (1, 1)),
              IntLiter(3, (1, 1)),
              (1, 1)
            ),
            (1, 1)
          )
        )
    )
  }

}
