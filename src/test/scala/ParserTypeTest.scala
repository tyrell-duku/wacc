import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._

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
    assert(types.runParser("int[]").contains(OfArrayType(IntT)))
    assert(types.runParser("string[]").contains(OfArrayType(StringT)))
    assert(types.runParser("char[]").contains(OfArrayType(CharT)))
    assert(types.runParser("bool[]").contains(OfArrayType(BoolT)))
  }

  test("Successfully parses nested arrays") {
    assert(types.runParser("int[][]").contains(OfArrayType(OfArrayType(IntT))))
    assert(
      types
        .runParser("bool[][][]")
        .contains(OfArrayType(OfArrayType(OfArrayType(BoolT))))
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
          Pair(PairElemT(OfArrayType(IntT)), PairElemT(OfArrayType(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[],int[])")
        .contains(
          Pair(PairElemT(OfArrayType(BoolT)), PairElemT(OfArrayType(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[],int[])")
        .contains(
          Pair(PairElemT(OfArrayType(BoolT)), PairElemT(OfArrayType(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[][],int[])")
        .contains(
          Pair(
            PairElemT(OfArrayType(OfArrayType(BoolT))),
            PairElemT(OfArrayType(IntT))
          )
        )
    )
    assert(
      types
        .runParser("pair(char[][],int[])[]")
        .contains(
          OfArrayType(
            Pair(
              PairElemT(OfArrayType(OfArrayType(CharT))),
              PairElemT(OfArrayType(IntT))
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
