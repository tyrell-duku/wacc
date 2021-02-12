import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof
import LiterParser._
import StatParser._
import ExprParser._
import Lexer._

class ArrayElemTest extends AnyFunSuite {
  test("Successfully parses double array-elem") {
    assert(
      arrayElem
        .runParser("var[10][2]")
        .contains(
          ArrayElem(Ident("var"), List(IntLiter(10), IntLiter(2)))
        )
    )
  }

  test("Successfully fails to parse an array-elem without expr") {
    assert(arrayElem.runParser("var").isFailure)
  }

  test("Successfully fails to parse an empty array-elem") {
    assert(arrayElem.runParser("var[]").isFailure)
  }

  test("Successfully parses single element arrayElem") {
    assert(
      arrayElem
        .runParser("var[10]")
        .contains(ArrayElem(Ident("var"), List(IntLiter(10))))
    )
    assert(
      arrayElem
        .runParser("var[var[10]]")
        .contains(
          ArrayElem(
            Ident("var"),
            List(ArrayElem(Ident("var"), List(IntLiter(10))))
          )
        )
    )

  }
}

class PairElemTest extends AnyFunSuite {
  val pairElemWhitespace = lexer.whiteSpace *> pairElem <* eof

  test("Successfully parses fst") {
    assert(
      pairElem.runParser("fst(65)").contains(Fst(Parens(IntLiter(65))))
    )
    assert(
      pairElemWhitespace
        .runParser("fst 65")
        .contains(Fst(IntLiter(65)))
    )
  }

  test("Successfully fails to parse fst with non-valid expr") {
    assert(pairElem.runParser("fst(fst(90))").isFailure)
  }

  test("Successfully parses snd") {
    assert(
      pairElem.runParser("snd(5)").contains(Snd(Parens(IntLiter(5))))
    )
    assert(
      pairElemWhitespace
        .runParser(" snd  65")
        .contains(Snd(IntLiter(65)))
    )
  }

  test("Successfully fails to parse snd with non-valid expr") {
    assert(pairElem.runParser("fst(snd(fst(90)))").isFailure)
  }
}
