import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof

class NaturalTest extends AnyFunSuite {
  test("Successfully parses natural numbers") {
    assert(natural.runParser("100").contains(100))
    assert(natural.runParser("2").contains(2))
    assert(natural.runParser("0").contains(0))
    assert(natural.runParser("400").contains(400))
  }

  test("Successfully fails to parse decimal part of a number") {
    assert(natural.runParser("100.5").contains(100))
    assert(natural.runParser("0.5").contains(0))
  }
}

class IntSignTest extends AnyFunSuite {
  test("Successfully parses +") {
    assert(intSign.runParser("+").contains(Pos))
  }

  test("Successfully parses -") {
    assert(intSign.runParser("-").contains(Neg))
  }
}
