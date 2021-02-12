import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import LiterParser._
import ExprParser._
import StatParser._
import Lexer._

class BaseTypeTest extends AnyFunSuite {
  test("Successfully parses int type") {
    assert(baseType.runParser("int").contains(IntT))
  }

  test("Successfully parses bool type") {
    assert(baseType.runParser("bool").contains(BoolT))
  }

  test("Successfully parses char type") {
    assert(baseType.runParser("char").contains(CharT))
  }

  test("Successfully parses string type") {
    assert(baseType.runParser("string").contains(StringT))
  }

  test("Successfully fails to parse string 'float'") {
    assert(baseType.runParser("float").isFailure)
  }
}

class ArrayTypeTest extends AnyFunSuite {
  test("Successfully parses base type arrays") {
    assert(types.runParser("int[]").contains(ArrayT(IntT)))
    assert(types.runParser("string[]").contains(ArrayT(StringT)))
    assert(types.runParser("char[]").contains(ArrayT(CharT)))
    assert(types.runParser("bool[]").contains(ArrayT(BoolT)))
  }

  test("Successfully parses nested arrays") {
    assert(types.runParser("int[][]").contains(ArrayT(ArrayT(IntT))))
    assert(
      types
        .runParser("bool[][][]")
        .contains(ArrayT(ArrayT(ArrayT(BoolT))))
    )
  }
}

class PairTypeTest extends AnyFunSuite {
  test("Successfully parses pair of base types") {
    assert(
      types
        .runParser("pair(int,int)")
        .contains(Pair(PairElemT(IntT), PairElemT(IntT)))
    )
    assert(
      types
        .runParser("pair(char,int)")
        .contains(Pair(PairElemT(CharT), PairElemT(IntT)))
    )
  }

  test("Successfully parses pair of array-types") {
    assert(
      types
        .runParser("pair(int[],int[])")
        .contains(
          Pair(PairElemT(ArrayT(IntT)), PairElemT(ArrayT(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[],int[])")
        .contains(
          Pair(PairElemT(ArrayT(BoolT)), PairElemT(ArrayT(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[],int[])")
        .contains(
          Pair(PairElemT(ArrayT(BoolT)), PairElemT(ArrayT(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[][],int[])")
        .contains(
          Pair(
            PairElemT(ArrayT(ArrayT(BoolT))),
            PairElemT(ArrayT(IntT))
          )
        )
    )
    assert(
      types
        .runParser("pair(char[][],int[])[]")
        .contains(
          ArrayT(
            Pair(
              PairElemT(ArrayT(ArrayT(CharT))),
              PairElemT(ArrayT(IntT))
            )
          )
        )
    )
  }

  test("Successfully fails to parse pair-type within pair-type") {
    assert(types.runParser("pair(int,pair(char,int))").isFailure)
  }

  test("Successfully parses 'pair' within pair-type") {
    assert(
      types
        .runParser("pair(pair,pair)")
        .contains(Pair(PairElemPair, PairElemPair))
    )
  }
}
