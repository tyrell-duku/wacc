object Rules {

  sealed case class Program(fs: List[Func], s: Stat)

  sealed case class Func(
      t: Type,
      id: Ident,
      ps: Option[ParamList] = None,
      s: Stat
  )

  sealed case class ParamList(ps: List[Param])

  sealed case class Param(t: Type, id: Ident)

  sealed trait Stat
  case object Skip extends Stat
  case class EqIdent(t: Type, id: Ident, aRHS: AssignRHS) extends Stat
  case class EqAssign(aLHS: AssignLHS, aRHS: AssignRHS) extends Stat
  case class Read(aLHS: AssignLHS) extends Stat
  case class Free(e: Expr) extends Stat
  case class Return(e: Expr) extends Stat
  case class Exit(e: Expr) extends Stat
  case class Print(e: Expr) extends Stat
  case class PrintLn(e: Expr) extends Stat
  case class If(cond: Expr, s1: Stat, s2: Stat) extends Stat
  case class While(cond: Expr, s: Stat) extends Stat
  case class Begin(s: Stat) extends Stat
  case class Seq(stats: List[Stat]) extends Stat

  sealed trait AssignLHS

  sealed trait AssignRHS {
    def getType(sTable: SymbolTable): Type = Err

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
  case class Newpair(fst: Expr, snd: Expr) extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      Pair(PairElemT(fst.getType(sTable)), PairElemT(snd.getType(sTable)))
    }
  }
  // check correct param list?
  case class Call(id: Ident, args: Option[ArgList] = None) extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      id.getType(sTable)
    }
  }

  sealed case class ArgList(args: List[Expr])

  sealed trait PairElem extends AssignLHS with AssignRHS
  case class Fst(e: Expr) extends PairElem
  case class Snd(e: Expr) extends PairElem

  sealed trait Type

  case object Err extends Type

  sealed trait BaseType extends Type
  case object IntT extends BaseType {
    override def toString(): String = "int"
  }
  case object BoolT extends BaseType {
    override def toString: String = "bool"
  }
  case object CharT extends BaseType {
    override def toString: String = "char"
  }
  case object StringT extends BaseType {
    override def toString: String = "string"
  }

  sealed case class ArrayT(t: Type) extends Type {
    override def toString: String = t + "[]"
  }

  sealed trait PairType extends Type
  case class Pair(x: PairElemType, y: PairElemType) extends PairType

  sealed trait PairElemType
  case object PairElemPair extends PairElemType {
    override def toString: String = "pair"
  }
  case class PairElemT(t: Type) extends PairElemType {
    override def toString: String = t.toString
  }

  sealed trait Expr extends AssignRHS
  case class Parens(e: Expr) extends Expr {
    override def getType(sTable: SymbolTable): Type =
      e.getType(sTable)
  }

  sealed trait UnOp extends Expr {
    val e: Expr
    val expected: (Type, Type)

    override def getType(sTable: SymbolTable): Type = {
      val actual = e.getType(sTable)
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

    override def getType(sTable: SymbolTable): Type = {
      val actual = e.getType(sTable)
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
    val lExpr: Expr
    val rExpr: Expr
    val expected: (List[Type], Type)

    override def getType(sTable: SymbolTable): Type = {
      val actualL = lExpr.getType(sTable)
      val actualR = rExpr.getType(sTable)
      if (actualL == actualR) {
        if (expected._1.contains(actualL) && expected._1.contains(actualR))
          return expected._2
        if (expected._1.isEmpty) return expected._2
      } else {
        if (expected._1.contains(actualL))
          return typeErr(rExpr, actualR, List(actualL))
        if (expected._1.contains(actualR))
          return typeErr(lExpr, actualL, List(actualR))
      }
      // neither types are correct
      typeErr(lExpr, actualL, expected._1)
      typeErr(rExpr, actualR, expected._1)
    }
  }

  sealed trait ArithOps extends BinOp {
    override val expected: (List[Type], Type) = (List(IntT), IntT)
  }
  case class Mul(lExpr: Expr, rExpr: Expr) extends ArithOps
  case class Div(lExpr: Expr, rExpr: Expr) extends ArithOps
  case class Mod(lExpr: Expr, rExpr: Expr) extends ArithOps
  case class Plus(lExpr: Expr, rExpr: Expr) extends ArithOps
  case class Sub(lExpr: Expr, rExpr: Expr) extends ArithOps

  sealed trait ComparOps extends BinOp {
    override val expected: (List[Type], Type) = (List(CharT, IntT), BoolT)
  }
  case class GT(lExpr: Expr, rExpr: Expr) extends ComparOps
  case class GTE(lExpr: Expr, rExpr: Expr) extends ComparOps
  case class LT(lExpr: Expr, rExpr: Expr) extends ComparOps
  case class LTE(lExpr: Expr, rExpr: Expr) extends ComparOps

  sealed trait EqOps extends BinOp {
    override val expected: (List[Type], Type) = (List.empty, BoolT)
  }
  case class Equal(lExpr: Expr, rExpr: Expr) extends EqOps
  case class NotEqual(lExpr: Expr, rExpr: Expr) extends EqOps

  sealed trait LogicalOps extends BinOp {
    override val expected: (List[Type], Type) = (List(BoolT), BoolT)
  }
  case class And(lExpr: Expr, rExpr: Expr) extends LogicalOps
  case class Or(lExpr: Expr, rExpr: Expr) extends LogicalOps

  sealed case class Ident(s: String)
      extends AssignLHS
      with AssignRHS
      with Expr {
    override def getType(sTable: SymbolTable): Type = {
      if (!sTable.contains(this)) {
        println("Variable " + s + " undeclared/ not in scope")
        return Err
      }
      val Meta(typeOf, _) = sTable.lookupAll(this)
      typeOf
    }
  }

  // TODO: Check only ints in list y?, length of array (runtime error)
  sealed case class ArrayElem(id: Ident, exprs: List[Expr])
      extends AssignLHS
      with Expr {
    override def getType(sTable: SymbolTable): Type = {
      val actual = id.getType(sTable)
      actual match {
        case ArrayT(inside) => inside
        case _              => typeErr(id, actual, List(ArrayT(actual)))
      }
    }
  }

  sealed case class IntLiter(n: Int) extends Expr {
    override def getType(sTable: SymbolTable): Type = IntT
  }

  sealed trait IntSign
  case object Pos extends IntSign
  case object Neg extends IntSign

  sealed case class BoolLiter(b: Boolean) extends Expr {
    override def getType(sTable: SymbolTable): Type = StringT
  }

  sealed case class CharLiter(c: Character) extends Expr {
    override def getType(sTable: SymbolTable): Type = CharT
  }

  sealed trait Character
  case class NormalChar(c: Char) extends Character
  case class Escape(c: Char) extends Character

  sealed case class StrLiter(str: List[Character]) extends Expr {
    override def getType(sTable: SymbolTable): Type = StringT
  }

  sealed case class ArrayLiter(arr: Option[List[Expr]]) extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      if (arr.isEmpty) {
        return ArrayT(null)
      }
      val arrLitTypes = arr.get.map(_.getType(sTable))
      if (arrLitTypes.forall(_ == arrLitTypes.head)) {
        return ArrayT(arrLitTypes.head)
      }
      Err
    }
  }

  // TODO: pair liter check?
  sealed case class PairLiter() extends Expr {
    override def getType(sTable: SymbolTable): Type =
      Pair(null, null)
  }

}
