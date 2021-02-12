import Rules.IntT
import Rules.BoolT
import Rules.CharT
import Rules.StringT
import Rules.ArrayT
import Rules.Pair
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
    var semErrs: List[SemanticError] = List.empty[SemanticError]
  }

  case class Newpair(fst: Expr, snd: Expr) extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      val fstType = fst.getType(sTable)
      val sndType = snd.getType(sTable)
      semErrs = fst.semErrs ::: snd.semErrs
      var fstPairElem: PairElemType = PairElemPair
      if (!fstType.isPair) {
        fstPairElem = PairElemT(fstType)
      }
      var sndPairElem: PairElemType = PairElemPair
      if (!sndType.isPair) {
        sndPairElem = PairElemT(sndType)
      }

      Pair(fstPairElem, sndPairElem)
    }
  }

  case class Call(id: Ident, args: Option[ArgList] = None) extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      val idType = id.getType(sTable)
      semErrs :::= sTable.funcParamMatch(id, args)
      idType
    }
    override def toString: String =
      id + "(" + args.getOrElse(ArgList(List())) + ")"
  }

  sealed case class ArgList(args: List[Expr]) {
    override def toString(): String = {
      args.mkString(", ")
    }
  }

  sealed trait PairElem extends AssignLHS with AssignRHS {
    val e: Expr
  }
  case class Fst(e: Expr) extends PairElem {
    override def toString: String = "fst " + e

    override def getType(sTable: SymbolTable): Type = {
      e match {
        case Ident(_) =>
          val eType = e.getType(sTable)
          eType match {
            case Pair(fst, snd) => return fst.getType
            case _              =>
          }
        case _ =>
      }
      semErrs ::= invalidPairElem(this)
      Any
    }
  }

  case class Snd(e: Expr) extends PairElem {
    override def toString: String = "snd " + e

    override def getType(sTable: SymbolTable): Type = {
      e match {
        case Ident(_) =>
          val eType = e.getType(sTable)
          eType match {
            case Pair(_, snd) => return snd.getType
            case _            =>
          }
        case _ =>
      }
      semErrs ::= invalidPairElem(this)
      Any
    }
  }

  sealed trait Type {
    def isArray: Boolean = this match {
      case ArrayT(_) => true
      case _         => false
    }
    def isPair: Boolean = this match {
      case Pair(_, _) => true
      case _          => false
    }
  }

  case object Any extends Type {
    override def equals(x: Any): Boolean =
      x match {
        case t: Type => true
        case _       => false
      }
  }

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
      "Pair(" + x + "," + y + ")"
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
    override def toString: String = "(" + e + ")"
    override def getType(sTable: SymbolTable): Type = {
      val retType = e.getType(sTable)
      semErrs = e.semErrs
      retType
    }
  }

  sealed trait UnOp extends Expr {
    val e: Expr
    val expected: (Type, Type)
    val unOperatorStr: String

    override def getType(sTable: SymbolTable): Type = {
      val actual = e.getType(sTable)
      semErrs = e.semErrs
      if (actual != expected._1) {
        semErrs ::= typeMismatch(e, actual, List(expected._1))
      }
      expected._2
    }

    override def toString: String = unOperatorStr + e.toString
  }

  case class Not(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (BoolT, BoolT)
    val unOperatorStr = "!"
  }
  case class Negation(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (IntT, IntT)
    val unOperatorStr = "-"
  }
  case class Len(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (ArrayT(null), IntT)
    val unOperatorStr = "len "

    override def getType(sTable: SymbolTable): Type = {
      val actual = e.getType(sTable)
      semErrs = e.semErrs
      if (!actual.isArray) {
        semErrs ::= typeMismatch(e, actual, List(expected._1))
      }
      expected._2
    }
  }
  case class Ord(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (CharT, IntT)
    val unOperatorStr = "ord "
  }
  case class Chr(e: Expr) extends UnOp {
    override val expected: (Type, Type) = (IntT, CharT)
    val unOperatorStr = "chr "
  }

  sealed trait BinOp extends Expr {
    val lExpr: Expr
    val rExpr: Expr
    val expected: (List[Type], Type)
    val operatorStr: String

    override def toString: String = lExpr + " " + operatorStr + " " + rExpr

    override def getType(sTable: SymbolTable): Type = {
      val actualL = lExpr.getType(sTable)
      val actualR = rExpr.getType(sTable)
      semErrs = lExpr.semErrs ::: rExpr.semErrs
      if (actualL == Any || actualR == Any) {
        return expected._2
      }
      if (actualL == actualR) {
        if (expected._1.contains(actualL) && expected._1.contains(actualR))
          return expected._2
        if (expected._1.isEmpty) {
          return expected._2
        }
      } else {
        if (expected._1.contains(actualL)) {
          semErrs ::= typeMismatch(rExpr, actualR, List(actualL))
          return expected._2
        }
        if (expected._1.contains(actualR)) {
          semErrs ::= typeMismatch(lExpr, actualL, List(actualR))
          return expected._2
        }
      }
      // neither types are correct
      semErrs ::= typeMismatch(lExpr, actualL, expected._1)
      semErrs ::= typeMismatch(rExpr, actualR, expected._1)
      expected._2

    }
  }

  sealed trait ArithOps extends BinOp {
    override val expected: (List[Type], Type) = (List(IntT), IntT)
  }
  case class Mul(lExpr: Expr, rExpr: Expr) extends ArithOps {
    val operatorStr = "*"
  }
  case class Div(lExpr: Expr, rExpr: Expr) extends ArithOps {
    val operatorStr = "/"
  }
  case class Mod(lExpr: Expr, rExpr: Expr) extends ArithOps {
    val operatorStr = "%"
  }
  case class Plus(lExpr: Expr, rExpr: Expr) extends ArithOps {
    val operatorStr = "+"
  }
  case class Sub(lExpr: Expr, rExpr: Expr) extends ArithOps {
    val operatorStr = "-"
  }

  sealed trait ComparOps extends BinOp {
    override val expected: (List[Type], Type) = (List(CharT, IntT), BoolT)
  }
  case class GT(lExpr: Expr, rExpr: Expr) extends ComparOps {
    val operatorStr = ">"
  }
  case class GTE(lExpr: Expr, rExpr: Expr) extends ComparOps {
    val operatorStr = ">="
  }
  case class LT(lExpr: Expr, rExpr: Expr) extends ComparOps {
    val operatorStr = "<"
  }
  case class LTE(lExpr: Expr, rExpr: Expr) extends ComparOps {
    val operatorStr = "<="
  }

  sealed trait EqOps extends BinOp {
    override val expected: (List[Type], Type) = (List.empty, BoolT)
  }
  case class Equal(lExpr: Expr, rExpr: Expr) extends EqOps {
    val operatorStr = "=="
  }
  case class NotEqual(lExpr: Expr, rExpr: Expr) extends EqOps {
    val operatorStr = "!="
  }

  sealed trait LogicalOps extends BinOp {
    override val expected: (List[Type], Type) = (List(BoolT), BoolT)
  }
  case class And(lExpr: Expr, rExpr: Expr) extends LogicalOps {
    val operatorStr = "&&"
  }
  case class Or(lExpr: Expr, rExpr: Expr) extends LogicalOps {
    val operatorStr = "||"
  }

  sealed case class Ident(s: String)
      extends AssignLHS
      with AssignRHS
      with Expr {

    override def toString: String = s

    override def getType(sTable: SymbolTable): Type = {
      if (!sTable.contains(this)) {
        semErrs ::= variableNotDeclared(this)
        return Any
      }
      val Meta(typeOf, _) = sTable.lookupAll(this)
      typeOf
    }
  }

  sealed case class ArrayElem(id: Ident, exprs: List[Expr])
      extends AssignLHS
      with Expr {

    override def getType(sTable: SymbolTable): Type = {
      val actual = id.getType(sTable)
      semErrs = id.semErrs
      if (actual.isArray) {
        val ArrayT(innerT) = actual
        return innerT
      }
      semErrs ::= typeMismatch(id, actual, List(ArrayT(actual)))
      actual
    }
  }

  sealed case class IntLiter(n: Int) extends Expr {
    override def toString: String = n.toString
    override def getType(sTable: SymbolTable): Type = IntT
  }

  sealed trait IntSign
  case object Pos extends IntSign
  case object Neg extends IntSign

  sealed case class BoolLiter(b: Boolean) extends Expr {
    override def toString: String = b.toString
    override def getType(sTable: SymbolTable): Type = BoolT
  }

  sealed case class CharLiter(c: Character) extends Expr {
    override def toString: String = "'" + c.toString + "'"
    override def getType(sTable: SymbolTable): Type = CharT
  }

  sealed trait Character
  case class NormalChar(c: Char) extends Character {
    override def toString: String = c.toString
  }
  case class Escape(c: Char) extends Character {
    override def toString: String = s"\\$c"
  }

  sealed case class StrLiter(str: List[Character]) extends Expr {
    override def toString: String = "\"" + str.mkString("") + "\""
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
      ArrayT(null)
    }
  }

  sealed case class PairLiter() extends Expr {
    override def toString: String = "null"
    override def getType(sTable: SymbolTable): Type =
      Pair(null, null)
  }
}
