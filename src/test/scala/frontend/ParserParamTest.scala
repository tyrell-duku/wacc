import org.scalatest.funsuite.AnyFunSuite
import frontend.Parser._
import frontend.Rules._
import parsley.combinator.eof
import frontend.Lexer._

class ParserParamTest extends AnyFunSuite {
  val paramWhitespace = lexer.whiteSpace *> param <* eof

  test("Successfully parses param with base-type") {
    assert(
      paramWhitespace
        .runParser("int var")
        .contains(Param(IntT, Ident("var", (1, 5))))
    )
    assert(
      paramWhitespace
        .runParser("bool var")
        .contains(Param(BoolT, Ident("var", (1, 6))))
    )
  }

  test("Successfully parses param with array-type") {
    assert(
      paramWhitespace
        .runParser("char[][] var")
        .contains(Param(ArrayT(ArrayT(CharT)), Ident("var", (1, 10))))
    )
    assert(
      paramWhitespace
        .runParser("int[] var")
        .contains(Param(ArrayT(IntT), Ident("var", (1, 7))))
    )
  }

  test("Successfully parses param with pair-type") {
    assert(
      paramWhitespace
        .runParser("pair(int,char) var")
        .contains(
          Param(Pair(PairElemT(IntT), PairElemT(CharT)), Ident("var", (1, 16)))
        )
    )
    assert(
      paramWhitespace
        .runParser("pair(pair,pair)[] var")
        .contains(
          Param(ArrayT(Pair(PairElemPair, PairElemPair)), Ident("var", (1, 19)))
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
              Param(
                Pair(PairElemT(IntT), PairElemT(CharT)),
                Ident("var", (1, 16))
              )
            )
          )
        )
    )
    assert(
      paramWhitespace
        .runParser("bool var")
        .contains(ParamList(List(Param(BoolT, Ident("var", (1, 6))))))
    )
  }

  test("Successfully parses param-list") {
    assert(
      paramWhitespace
        .runParser("int var1, char var2, bool var3")
        .contains(
          ParamList(
            List(
              Param(IntT, Ident("var1", (1, 5))),
              Param(CharT, Ident("var2", (1, 16))),
              Param(BoolT, Ident("var3", (1, 27)))
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
              Param(ArrayT(IntT), Ident("x", (1, 7))),
              Param(ArrayT(IntT), Ident("y", (1, 16))),
              Param(ArrayT(IntT), Ident("z", (1, 25)))
            )
          )
        )
    )

  }

  test("Successfully fails to parse empty param list") {
    assert(paramWhitespace.runParser("").isFailure)
  }
}
