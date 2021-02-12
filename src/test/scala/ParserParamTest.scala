import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof

class ParamTest extends AnyFunSuite {
  val paramWhitespace = lexer.whiteSpace *> param <* eof

  test("Successfully parses param with base-type") {
    assert(
      paramWhitespace.runParser("int var").contains(Param(IntT, Ident("var")))
    )
    assert(
      paramWhitespace.runParser("bool var").contains(Param(BoolT, Ident("var")))
    )
  }

  test("Successfully parses param with array-type") {
    assert(
      paramWhitespace
        .runParser("char[][] var")
        .contains(Param(ArrayT(ArrayT(CharT)), Ident("var")))
    )
    assert(
      paramWhitespace
        .runParser("int[] var")
        .contains(Param(ArrayT(IntT), Ident("var")))
    )
  }

  test("Successfully parses param with pair-type") {
    assert(
      paramWhitespace
        .runParser("pair(int,char) var")
        .contains(Param(Pair(PairElemT(IntT), PairElemT(CharT)), Ident("var")))
    )
    assert(
      paramWhitespace
        .runParser("pair(pair,pair)[] var")
        .contains(
          Param(ArrayT(Pair(PairElemPair, PairElemPair)), Ident("var"))
        )
    )
  }

  test("Successfully fails to parse param with invalid ident") {
    assert(paramWhitespace.runParser("int 1var").isFailure)
    assert(paramWhitespace.runParser("bool[] *var").isFailure)
  }
}

class ParamListTest extends AnyFunSuite {
  val paramWhitespace = lexer.whiteSpace *> paramList <* eof

  test("Successfully parses singleton param-list") {
    assert(
      paramWhitespace
        .runParser("pair(int,char) var")
        .contains(
          ParamList(
            List(
              Param(Pair(PairElemT(IntT), PairElemT(CharT)), Ident("var"))
            )
          )
        )
    )
    assert(
      paramWhitespace
        .runParser("bool var")
        .contains(ParamList(List(Param(BoolT, Ident("var")))))
    )
  }

  test("Successfully parses param-list") {
    assert(
      paramWhitespace
        .runParser("int var1, char var2, bool var3")
        .contains(
          ParamList(
            List(
              Param(IntT, Ident("var1")),
              Param(CharT, Ident("var2")),
              Param(BoolT, Ident("var3"))
            )
          )
        )
    )
    assert(
      paramWhitespace
        .runParser("int[] x, int[] y, int[] z")
        .contains(
          ParamList(
            List(
              Param(ArrayT(IntT), Ident("x")),
              Param(ArrayT(IntT), Ident("y")),
              Param(ArrayT(IntT), Ident("z"))
            )
          )
        )
    )

  }

  test("Successfully fails to parse empty param list") {
    assert(paramWhitespace.runParser("").isFailure)
  }
}
