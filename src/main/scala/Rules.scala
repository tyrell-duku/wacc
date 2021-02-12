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
    def getType(sTable: SymbolTable): Type
  }
  // TODO: Newpair, Call, fst, snd correct types?
  case class Newpair(fst: Expr, snd: Expr) extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      // val fstType = fst.getType(sTable)
      // val secType = snd.getType(sTable)
      // if (fstType.isErr) {
      //   if (sndType.isErr) {}
      // }
      Pair(PairElemT(fst.getType(sTable)), PairElemT(snd.getType(sTable)))
    }
  }
  // check correct param list?
  case class Call(id: Ident, args: Option[ArgList] = None) extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      id.getType(sTable)
      // val paramCheck = sTable.funcParamMatch(id, args)
      // if (paramCheck.isEmpty) {
      //   return idType
      // }
      // //println("Param mismatch")
      // Err(paramCheck)

    }
  }

  sealed case class ArgList(args: List[Expr])

  // TODO: type for fst and snd
  // PairElemT(CharT) | PairElemT
  sealed trait PairElem extends AssignLHS with AssignRHS {
    val e: Expr
  }
  case class Fst(e: Expr) extends PairElem {
    override def toString: String = "fst " + e

    override def getType(sTable: SymbolTable): Type = e match {
      case Ident(_) =>
        val eType = e.getType(sTable)
        eType match {
          case Pair(fst, _) => fst.getType
          case _            => Err(List(invalidPairElem(e)))
        }
      case _ => Err(List(invalidPairElem(e)))
    }
  }

  case class Snd(e: Expr) extends PairElem {
    override def toString: String = "snd " + e

    override def getType(sTable: SymbolTable): Type = e match {
      case Ident(_) =>
        val eType = e.getType(sTable)
        eType match {
          case Pair(_, snd) => snd.getType
          case _            => Err(List(invalidPairElem(e)))
        }
      case _ => Err(List(invalidPairElem(e)))
    }
  }

  sealed trait Type {
    def isErr: Boolean = this match {
      case Err(_) => true
      case _      => false
    }
  }

  case class Err(semErrs: List[SemanticError]) extends Type

  sealed trait BaseType extends Type
  case object IntT extends BaseType {
    override def toString: String = "Int"
  }
  case object BoolT extends BaseType {
    override def toString: String = "Bool"
  }
  case object CharT extends BaseType {
    override def toString: String = "Char"
  }
  case object StringT extends BaseType {
    override def toString: String = "String"
  }

  sealed case class ArrayT(t: Type) extends Type {
    override def toString: String = {
      if (t == null) return "T[]"
      t + "[]"
    }
  }

  sealed trait PairType extends Type
  case class Pair(x: PairElemType, y: PairElemType) extends PairType {
    override def toString: String = {
      if ((x == null) && (y == null)) {
        return "Pair"
      }
      return "Pair(" + x + "," + y + ")"
    }
  }

  sealed trait PairElemType {
    def getType: Type
  }
  case object PairElemPair extends PairElemType {
    override def toString: String = "pair"

    override def getType: Type = Pair(null, null)
  }
  case class PairElemT(t: Type) extends PairElemType {
    override def toString: String = t.toString

    override def getType: Type = t
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
      Err(List(typeMismatch(e, actual, List(expected._1))))
    }

    def unOpstring(op: String): String = op + e.toString()
  }

  case class Not(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (BoolT, BoolT)
    override def toString(): String = this.unOpstring("!")
  }
  case class Negation(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (IntT, IntT)
    override def toString(): String = this.unOpstring("-")
  }
  case class Len(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (ArrayT(null), IntT)
    override def toString(): String = this.unOpstring("len ")

    override def getType(sTable: SymbolTable): Type = {
      val actual = e.getType(sTable)
      actual match {
        case ArrayT(_) => expected._2
        case _         => Err(List(typeMismatch(e, actual, List(expected._1))))
      }
    }
  }
  case class Ord(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (CharT, IntT)
    override def toString(): String = this.unOpstring("ord ")
  }
  case class Chr(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (IntT, CharT)
    override def toString(): String = this.unOpstring("chr ")
  }

  sealed trait BinOp extends Expr {
    val lExpr: Expr
    val rExpr: Expr
    val expected: (List[Type], Type)

    def binOpString(op: String): String = lExpr + " " + op + " " + rExpr

    override def getType(sTable: SymbolTable): Type = {
      val actualL = lExpr.getType(sTable)
      val actualR = rExpr.getType(sTable)
      if (actualL == actualR) {
        if (expected._1.contains(actualL) && expected._1.contains(actualR))
          return expected._2
        if (expected._1.isEmpty) {
          return expected._2
        }
      } else {
        if (expected._1.contains(actualL))
          return Err(List(typeMismatch(rExpr, actualR, List(actualL))))
        if (expected._1.contains(actualR))
          return Err(List(typeMismatch(lExpr, actualL, List(actualR))))
      }
      // neither types are correct
      Err(
        List(
          typeMismatch(lExpr, actualL, expected._1),
          typeMismatch(rExpr, actualR, expected._1)
        )
      )
    }
  }

  sealed trait ArithOps extends BinOp {
    override val expected: (List[Type], Type) = (List(IntT), IntT)
  }
  case class Mul(lExpr: Expr, rExpr: Expr) extends ArithOps {
    override def toString(): String = this.binOpString("*")
  }
  case class Div(lExpr: Expr, rExpr: Expr) extends ArithOps {
    override def toString(): String = this.binOpString("/")
  }
  case class Mod(lExpr: Expr, rExpr: Expr) extends ArithOps {
    override def toString(): String = this.binOpString("%")
  }
  case class Plus(lExpr: Expr, rExpr: Expr) extends ArithOps {
    override def toString(): String = this.binOpString("+")
  }
  case class Sub(lExpr: Expr, rExpr: Expr) extends ArithOps {
    override def toString(): String = this.binOpString("-")
  }

  sealed trait ComparOps extends BinOp {
    override val expected: (List[Type], Type) = (List(CharT, IntT), BoolT)
  }
  case class GT(lExpr: Expr, rExpr: Expr) extends ComparOps {
    override def toString(): String = this.binOpString(">")
  }
  case class GTE(lExpr: Expr, rExpr: Expr) extends ComparOps {
    override def toString(): String = this.binOpString(">=")
  }
  case class LT(lExpr: Expr, rExpr: Expr) extends ComparOps {
    override def toString(): String = this.binOpString("<")
  }
  case class LTE(lExpr: Expr, rExpr: Expr) extends ComparOps {
    override def toString(): String = this.binOpString("<=")
  }

  sealed trait EqOps extends BinOp {
    override val expected: (List[Type], Type) = (List.empty, BoolT)
  }
  case class Equal(lExpr: Expr, rExpr: Expr) extends EqOps {
    override def toString(): String = this.binOpString("==")
  }
  case class NotEqual(lExpr: Expr, rExpr: Expr) extends EqOps {
    override def toString(): String = this.binOpString("!=")
  }

  sealed trait LogicalOps extends BinOp {
    override val expected: (List[Type], Type) = (List(BoolT), BoolT)
  }
  case class And(lExpr: Expr, rExpr: Expr) extends LogicalOps {
    override def toString(): String = this.binOpString("&&")
  }
  case class Or(lExpr: Expr, rExpr: Expr) extends LogicalOps {
    override def toString(): String = this.binOpString("||")
  }

  sealed case class Ident(s: String)
      extends AssignLHS
      with AssignRHS
      with Expr {

    override def getType(sTable: SymbolTable): Type = {
      if (!sTable.contains(this)) {
        //println("Variable " + s + " undeclared/ not in scope")
        return Err(List(variableNotDeclared(this)))
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
        case _              => Err(List(typeMismatch(id, actual, List(ArrayT(actual)))))
      }
      actual
    }
  }

  sealed case class IntLiter(n: Int) extends Expr {
    override def getType(sTable: SymbolTable): Type = IntT
  }

  sealed trait IntSign
  case object Pos extends IntSign
  case object Neg extends IntSign

  sealed case class BoolLiter(b: Boolean) extends Expr {
    override def getType(sTable: SymbolTable): Type = BoolT
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
      //to do
      // Err
      ArrayT(null)
    }
  }

  // TODO: pair liter check?
  sealed case class PairLiter() extends Expr {
    override def getType(sTable: SymbolTable): Type =
      Pair(null, null)
  }

}
