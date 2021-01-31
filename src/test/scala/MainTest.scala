import org.scalatest.funsuite.AnyFunSuite

class MainTest extends AnyFunSuite {

  test("succ increments by 1 correctly") {
    assert(Main.succ(10) == 11)
  }
}
