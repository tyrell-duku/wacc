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
    def getType(hMap : mutable.HashMap[Ident, Type]) : Type = NA

    def typeChecker(b: Boolean, t: Type): Type = {
      if (b) t else NA
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

  sealed trait BaseType extends Type
  case object IntT extends BaseType
  case object BoolT extends BaseType
  case object CharT extends BaseType
  case object StringT extends BaseType

  case object NA extends Type

  sealed case class ArrayT(x: Type) extends Type

  sealed trait PairType extends Type
  case class Pair(x: PairElemType, y: PairElemType) extends PairType

  sealed trait PairElemType
  case object PairElemPair extends PairElemType
  case class PairElemT(x: Type) extends PairElemType

  sealed trait Expr extends AssignRHS
  case class Parens(x: Expr) extends Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      x.getType(hMap)
    }
  }

  sealed trait UnOp extends Expr
  case class Not(x: Expr) extends UnOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      typeChecker(x.getType(hMap) == BoolT, BoolT)
    }
  }
  case class Negation(x: Expr) extends UnOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      typeChecker(x.getType(hMap) == IntT, IntT)
    }
  }

  case class Len(x: Expr) extends UnOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      x.getType(hMap) match {
        case ArrayT(_) => IntT
        case _  =>  NA
      }
    }
  }
  case class Ord(x: Expr) extends UnOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      typeChecker(x.getType(hMap) == CharT, IntT)
    }
  }
  case class Chr(x: Expr) extends UnOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      typeChecker(x.getType(hMap) == IntT, CharT)
    }
  }

  sealed trait BinOp extends Expr {
    val l : Expr
    val r : Expr
  }
  sealed trait ArithOps extends BinOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      typeChecker((l.getType(hMap) == IntT) && (r.getType(hMap) == IntT), IntT)
    }
  }
  case class Mul(l: Expr, r: Expr) extends ArithOps
  case class Div(l: Expr, r: Expr) extends ArithOps
  case class Mod(l: Expr, r: Expr) extends ArithOps
  case class Plus(l: Expr, r: Expr) extends ArithOps
  case class Sub(l: Expr, r: Expr) extends ArithOps

  sealed trait ComparOps extends BinOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      typeChecker((l.getType(hMap) == r.getType(hMap)) && ((l.getType(hMap) == IntT) || (l.getType(hMap) == CharT)), BoolT)
    }
  }
  case class GT(l: Expr, r: Expr) extends ComparOps
  case class GTE(l: Expr, r: Expr) extends ComparOps
  case class LT(l: Expr, r: Expr) extends ComparOps
  case class LTE(l: Expr, r: Expr) extends ComparOps

  sealed trait EqOps extends BinOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      typeChecker(l.getType(hMap) == r.getType(hMap), BoolT)
    }
  }
  case class Equal(l: Expr, r: Expr) extends EqOps
  case class NotEqual(l: Expr, r: Expr) extends EqOps

  sealed trait LogicalOps extends BinOp {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      typeChecker((l.getType(hMap) == BoolT) && (r.getType(hMap) == BoolT), BoolT)
    }
  }
  case class And(l: Expr, r: Expr) extends LogicalOps
  case class Or(l: Expr, r: Expr) extends LogicalOps

  sealed case class Ident(x: String) extends AssignLHS with AssignRHS with Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      if (!hMap.contains(this)){
        return NA
      }
      hMap.apply(this)
    }
  }

  // TO DO: Check only ints in list y?
  sealed case class ArrayElem(x: Ident, y: List[Expr])
      extends AssignLHS
      with Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      //y.map(getType(hMap))
      x.getType(hMap) match {
        case ArrayT(inside) => inside
        case _              => NA
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

  sealed case class ArrayLiter(x: Option[List[Expr]]) extends AssignRHS {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = {
      if (x.isEmpty) {
        return ArrayT(NA)
      }
      val arrayLiter = x.get
      val types = List[Type]()
      for (i <- arrayLiter.indices) {
        types :+ arrayLiter.apply(i).getType(hMap)
      }
      typeChecker(types.forall(_ == types.head), ArrayT(types.head))
    }
  }

  // To DO: confim correct type for pair liter
  sealed case class PairLiter() extends Expr {
    override def getType(hMap: mutable.HashMap[Ident, Type]): Type = Pair(null, null)
  }

}
