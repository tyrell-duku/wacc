import scala.collection.mutable

object Rules {

  sealed case class Program(fs: List[Func], s: Stat)

  sealed case class Func(
      t: Type,
      i: Ident,
      ps: Option[ParamList] = None,
      s: Stat
  )

  sealed case class ParamList(ps: List[Param])

  sealed case class Param(t: Type, i: Ident)

  sealed trait Stat
  case object Skip extends Stat
  case class EqIdent(t: Type, i: Ident, a: AssignRHS) extends Stat
  case class EqAssign(l: AssignLHS, r: AssignRHS) extends Stat
  case class Read(x: AssignLHS) extends Stat
  case class Free(x: Expr) extends Stat
  case class Return(x: Expr) extends Stat
  case class Exit(x: Expr) extends Stat
  case class Print(x: Expr) extends Stat
  case class PrintLn(x: Expr) extends Stat
  case class If(x: Expr, y: Stat, z: Stat) extends Stat
  case class While(x: Expr, y: Stat) extends Stat
  case class Begin(x: Stat) extends Stat
  case class Seq(x: List[Stat]) extends Stat

  sealed trait AssignLHS

  sealed trait AssignRHS {
    def getType(hMap: mutable.HashMap[Ident, Type]): Type = Err

    protected def typeErr(
        aRHS: AssignRHS,
        actual: Type,
        expect: List[Type]
    ): Type = {
      println(
        "Incompatible type at " + aRHS + " (expected type: " + expect
          .mkString(" or ") + ", actual type: " + actual
      )
      Err
    }
  }
  // TO DO: Newpair, Call, fst, snd correct types?
  case class Newpair(x: Expr, y: Expr) extends AssignRHS
  case class Call(x: Ident, y: Option[ArgList] = None) extends AssignRHS

  sealed case class ArgList(x: List[Expr])

  sealed trait PairElem extends AssignLHS with AssignRHS
  case class Fst(x: Expr) extends PairElem
  case class Snd(x: Expr) extends PairElem

  sealed trait Type

  case object Err extends Type

  sealed trait BaseType extends Type
  case object IntT extends BaseType
  case object BoolT extends BaseType
  case object CharT extends BaseType
  case object StringT extends BaseType

  sealed case class ArrayT(x: Type) extends Type

  sealed trait PairType extends Type
  case class Pair(x: PairElemType, y: PairElemType) extends PairType

  sealed trait PairElemType
  case object PairElemPair extends PairElemType
  case class PairElemT(x: Type) extends PairElemType

  sealed trait Expr extends AssignRHS
  case class Parens(e: Expr) extends Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type =
      e.getType(hMap)
  }

  sealed trait UnOp extends Expr {
    val e: Expr
    val expected: (Type, Type)

    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      val actual = e.getType(hMap)
      if (actual == expected._1) return expected._2
      typeErr(e, actual, List(expected._1))
    }
  }

  case class Not(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (BoolT, BoolT)
  }
  case class Negation(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (IntT, IntT)
  }
  case class Len(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (ArrayT(null), IntT)

    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      val actual = e.getType(hMap)
      actual match {
        case ArrayT(_) => expected._2
        case _         => typeErr(e, actual, List(expected._1))
      }
    }
  }
  case class Ord(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (CharT, IntT)
  }
  case class Chr(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (IntT, CharT)
  }

  sealed trait BinOp extends Expr {
    val l: Expr
    val r: Expr
    val expected: (List[Type], Type)

    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      val actualL = l.getType(hMap)
      val actualR = r.getType(hMap)
      if (actualL == actualR) {
        if (expected._1.contains(actualL) && expected._1.contains(actualR))
          return expected._2
        if (expected._1.isEmpty) return expected._2
      } else {
        if (expected._1.contains(actualL))
          return typeErr(r, actualR, List(actualL))
        if (expected._1.contains(actualR))
          return typeErr(l, actualL, List(actualR))
      }
      // neither types are correct
      typeErr(l, actualL, expected._1)
      typeErr(r, actualR, expected._1)
    }
  }

  sealed trait ArithOps extends BinOp {
    override val expected: (List[Type], Type) = (List(IntT), IntT)
  }
  case class Mul(l: Expr, r: Expr) extends ArithOps
  case class Div(l: Expr, r: Expr) extends ArithOps
  case class Mod(l: Expr, r: Expr) extends ArithOps
  case class Plus(l: Expr, r: Expr) extends ArithOps
  case class Sub(l: Expr, r: Expr) extends ArithOps

  sealed trait ComparOps extends BinOp {
    override val expected: (List[Type], Type) = (List(CharT, IntT), BoolT)
  }
  case class GT(l: Expr, r: Expr) extends ComparOps
  case class GTE(l: Expr, r: Expr) extends ComparOps
  case class LT(l: Expr, r: Expr) extends ComparOps
  case class LTE(l: Expr, r: Expr) extends ComparOps

  sealed trait EqOps extends BinOp {
    override val expected: (List[Type], Type) = (List.empty, BoolT)
  }
  case class Equal(l: Expr, r: Expr) extends EqOps
  case class NotEqual(l: Expr, r: Expr) extends EqOps

  sealed trait LogicalOps extends BinOp {
    override val expected: (List[Type], Type) = (List(BoolT), BoolT)
  }
  case class And(l: Expr, r: Expr) extends LogicalOps
  case class Or(l: Expr, r: Expr) extends LogicalOps

  sealed case class Ident(x: String)
      extends AssignLHS
      with AssignRHS
      with Expr {

    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      if (!hMap.contains(this)) {
        println("Variable " + x + " undeclared/ not in scope")
        return Err
      }
      hMap.apply(this)
    }
  }

  // TO DO: Check only ints in list y?, length of array (runtime error)
  sealed case class ArrayElem(x: Ident, y: List[Expr])
      extends AssignLHS
      with Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      val actual = x.getType(hMap)
      actual match {
        case ArrayT(inside) => inside
        case _              => typeErr(x, actual, List(ArrayT(null)))
      }
    }
  }

  sealed case class IntLiter(x: Int) extends Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = IntT
  }

  sealed trait IntSign
  case object Pos extends IntSign
  case object Neg extends IntSign

  sealed case class BoolLiter(x: Boolean) extends Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = StringT
  }

  sealed case class CharLiter(x: Character) extends Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = CharT
  }

  sealed trait Character
  case class NormalChar(x: Char) extends Character
  case class Escape(x: Char) extends Character

  sealed case class StrLiter(x: List[Character]) extends Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = StringT
  }

  sealed case class ArrayLiter(arr: Option[List[Expr]]) extends AssignRHS {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      if (arr.isEmpty)
        return Err //typeErrGenerator(this, ArrayLiter, List(ArrayT(null)))
      val arrLitTypes = arr.get.map(_.getType(hMap))
      if (arrLitTypes.forall(_ == arrLitTypes.head)) {
        return ArrayT(arrLitTypes.head)
      }
      Err
    }
  }

  // To DO: confim correct type for pair liter
  sealed case class PairLiter() extends Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type =
      Pair(null, null)
  }

}
