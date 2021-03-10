import org.scalatest.funsuite.AnyFunSuite
import frontend.HeapParser._
import frontend.LiterParser.{types}
import frontend.Rules._

class FrontendParserHeapTest extends AnyFunSuite {
  test("Successfully parses base pointer type") {
    assert(types.runParser("int*").contains(PtrT(IntT)))
    assert(types.runParser("bool*").contains(PtrT(BoolT)))
    assert(types.runParser("char*").contains(PtrT(CharT)))
    assert(types.runParser("string*").contains(PtrT(StringT)))
  }
  test("Successfully parses nested pointer type") {
    assert(types.runParser("int**").contains(PtrT(PtrT(IntT))))
    assert(types.runParser("bool**").contains(PtrT(PtrT(BoolT))))
    assert(types.runParser("char**").contains(PtrT(PtrT(CharT))))
    assert(types.runParser("string**").contains(PtrT(PtrT(StringT))))
  }
  test("Successfully parses array pointer type") {
    assert(types.runParser("int[]*").contains(PtrT(ArrayT(IntT))))
    assert(types.runParser("bool[]*").contains(PtrT(ArrayT(BoolT))))
    assert(types.runParser("char[]*").contains(PtrT(ArrayT(CharT))))
  }
  test("Successfully parses array of pointers") {
    assert(types.runParser("int*[]").contains(ArrayT(PtrT(IntT))))
    assert(types.runParser("bool*[]").contains(ArrayT(PtrT(BoolT))))
    assert(types.runParser("char*[]").contains(ArrayT(PtrT(CharT))))
  }
  test("Successfully parses pair pointer type") {
    assert(
      types
        .runParser("pair(int, int)*")
        .contains(PtrT(Pair(PairElemT(IntT), PairElemT(IntT))))
    )
    assert(
      types
        .runParser("pair(bool, bool)*")
        .contains(PtrT(Pair(PairElemT(BoolT), PairElemT(BoolT))))
    )
    assert(
      types
        .runParser("pair(char, char)*")
        .contains(PtrT(Pair(PairElemT(CharT), PairElemT(CharT))))
    )
    assert(
      types
        .runParser("pair(string, string)*")
        .contains(PtrT(Pair(PairElemT(StringT), PairElemT(StringT))))
    )
    assert(
      types
        .runParser("pair(pair, pair)*")
        .contains(PtrT(Pair(PairElemPair, PairElemPair)))
    )
    assert(
      types
        .runParser("pair(int[], int[])*")
        .contains(PtrT(Pair(PairElemT(ArrayT(IntT)), PairElemT(ArrayT(IntT)))))
    )
  }
  test("Successfully parses simple derefrenced pointer") {
    assert(
      derefPtr
        .runParser("*(ptr)")
        .contains(DerefPtr(Ident("ptr", (1, 3)), (1, 1)))
    )
    assert(
      derefPtr
        .runParser("*(var)")
        .contains(DerefPtr(Ident("var", (1, 3)), (1, 1)))
    )
  }
  test("Successfully fails to parse deref pointer with no parentheses") {
    assert(derefPtr.runParser("*ptr").isFailure)
    assert(derefPtr.runParser("*var").isFailure)
  }
  test("Successfully parses derefrenced pointer with arithmetic") {
    assert(
      derefPtr
        .runParser("*(ptr + 1)")
        .contains(
          DerefPtr(
            Plus(Ident("ptr", (1, 3)), IntLiter(1, (1, 9)), (1, 7)),
            (1, 1)
          )
        )
    )
  }

  test("Successfully parsers ampersand expression") {
    cancel("Code is still in construction.")
    // assert(addr.runParser("&x").contains(Addr(Ident("x", (1, 2)), (1, 1))))
  }
}
