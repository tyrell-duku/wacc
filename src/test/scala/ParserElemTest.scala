import org.scalatest.funsuite.AnyFunSuite
import frontend.Rules._
import parsley.combinator.eof
import ExprParser._
import Lexer._

class ArrayElemTest extends AnyFunSuite {
  test("Successfully parses double array-elem") {
    assert(
      arrayElem
        .runParser("var[10][2]")
        .contains(
          ArrayElem(Ident("var",(1,1)), List(IntLiter(10,(1,5)), IntLiter(2,(1,9))),(1,1))
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
        .contains(ArrayElem(Ident("var",(1,1)), List(IntLiter(10,(1,5))),(1,1)))
    )
    assert(
      arrayElem
        .runParser("var[var[10]]")
        .contains(
          ArrayElem(
            Ident("var",(1,1)),
            List(ArrayElem(Ident("var",(1,5)), List(IntLiter(10,(1,9))),(1,5))),
            (1,1)
          )
        )
    )

  }
}

class PairElemTest extends AnyFunSuite {
  val pairElemWhitespace = lexer.whiteSpace *> pairElem <* eof

  test("Successfully parses fst") {
    assert(
      pairElem.runParser("fst(65)").contains(Fst(IntLiter(65,(1,5)),(1,1)))
    )
    assert(
      pairElemWhitespace
        .runParser("fst 65")
        .contains(Fst(IntLiter(65,(1,5)),(1,1)))
    )
  }

  test("Successfully fails to parse fst with non-valid expr") {
    assert(pairElem.runParser("fst(fst(90))").isFailure)
  }

  test("Successfully parses snd") {
    assert(
      pairElem.runParser("snd(5)").contains(Snd(IntLiter(5,(1,5)),(1,1)))
    )
    assert(
      pairElemWhitespace
        .runParser(" snd  65")
        .contains(Snd(IntLiter(65,(1,7)),(1,2)))
    )
  }

  test("Successfully fails to parse snd with non-valid expr") {
    assert(pairElem.runParser("fst(snd(fst(90)))").isFailure)
  }
}
