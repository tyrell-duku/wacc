package frontend

import frontend.Semantics._
import parsley.Parsley
import parsley.Parsley.pos
import parsley.implicits.Map2
import scala.collection.mutable

object Rules {

  /* EXTENSION */

  // For runtime errors detected at compile time.
  sealed trait Runtime extends Expr
  // Integer overflow/underflow
  case object Overflow extends Runtime {
    override val pos = null
    override def getType(sTable: SymbolTable) = null
  }
  // Divide by zero
  case object ZeroDivision extends Runtime {
    override val pos = null
    override def getType(sTable: SymbolTable) = null
  }
  // Shifting with negative numbers
  case object NegShift extends Runtime {
    override val pos = null
    override def getType(sTable: SymbolTable) = null
  }
  // Arrary out of bounds check
  case object Bounds extends Runtime {
    override val pos = null
    override def getType(sTable: SymbolTable) = null
  }
  // Reference to null
  case object NullRef extends Runtime {
    override val pos = null
    override def getType(sTable: SymbolTable) = null
  }

  case class RuntimeErr(err: Runtime) extends Stat

  sealed trait MemoryAlloc extends AssignRHS
  case class Malloc(size: Expr, pos: (Int, Int)) extends MemoryAlloc {
    override def getType(sTable: SymbolTable): Type = {
      val t = size.getType(sTable)
      semErrs = size.semErrs
      if (t != IntT) {
        semErrs += TypeMismatch(size, t, List(IntT))
      }
      PtrT(null)
    }
  }
  object Malloc {
    def apply(size: Parsley[Expr]): Parsley[Malloc] =
      pos <**> size.map((size: Expr) => (p: (Int, Int)) => Malloc(size, p))
  }

  case class Realloc(ptr: Ident, size: Expr, pos: (Int, Int))
      extends MemoryAlloc {
    override def getType(sTable: SymbolTable): Type = {
      val t = size.getType(sTable)
      semErrs = size.semErrs
      if (t != IntT) {
        semErrs += TypeMismatch(size, t, List(IntT))
      }
      val ptrT = ptr.getType(sTable)
      semErrs ++= ptr.semErrs
      if (ptrT != PtrT(null)) {
        semErrs += TypeMismatch(ptr, ptrT, List(PtrT(null)))
      }
      ptrT
    }
  }
  object Realloc {
    def apply(ptr: Parsley[Ident], size: Parsley[Expr]): Parsley[Realloc] =
      pos <**> (ptr, size).map((ptr: Ident, size: Expr) =>
        (p: (Int, Int)) => Realloc(ptr, size, p)
      )
  }

  case class Calloc(num: Expr, size: Expr, pos: (Int, Int))
      extends MemoryAlloc {
    override def getType(sTable: SymbolTable): Type = {
      val t = size.getType(sTable)
      semErrs = size.semErrs
      if (t != IntT) {
        semErrs += TypeMismatch(size, t, List(IntT))
      }
      val numT = num.getType(sTable)
      semErrs ++= num.semErrs
      if (numT != IntT) {
        semErrs += TypeMismatch(num, numT, List(IntT))
      }
      PtrT(null)
    }
  }
  object Calloc {
    def apply(num: Parsley[Expr], size: Parsley[Expr]): Parsley[Calloc] =
      pos <**> (num, size).map((num: Expr, size: Expr) =>
        (p: (Int, Int)) => Calloc(num, size, p)
      )
  }

  case class PtrT(t: Type) extends Type {
    override def equals(x: Any): Boolean = x match {
      case PtrT(null)  => true
      case PtrT(inner) => if (t == null) true else inner == t
      case _           => false
    }
  }

  case class DerefPtr(ptr: Expr, pos: (Int, Int)) extends Expr with AssignLHS {
    override def getType(sTable: SymbolTable): Type = {
      val t = ptr.getType(sTable)
      semErrs = ptr.semErrs
      t match {
        case PtrT(inner) => inner
        case _ =>
          semErrs += TypeMismatch(ptr, t, List(PtrT(null)))
          null
      }
    }
    override def map[B >: AssignRHS](f: Expr => Expr) = DerefPtr(f(ptr), pos)
  }
  object DerefPtr {
    def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      pos.map((p: (Int, Int)) => (e: Expr) => DerefPtr(e, p)) <* op
  }

  case class Addr(ptr: Expr, pos: (Int, Int)) extends Expr {
    override def getType(sTable: SymbolTable): Type = {
      val t = ptr.getType(sTable)
      semErrs = ptr.semErrs
      PtrT(t)
    }
    override def map[B >: AssignRHS](f: Expr => Expr) = Addr(f(ptr), pos)
  }
  object Addr {
    def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      pos.map((p: (Int, Int)) => (e: Expr) => Addr(e, p)) <* op
  }

  case class SizeOf(t: Type, pos: (Int, Int)) extends Expr {
    override def getType(sTable: SymbolTable): Type = {
      IntT
    }
  }
  object SizeOf {
    def apply(t: Parsley[Type]): Parsley[SizeOf] =
      pos <**> t.map((t: Type) => (p: (Int, Int)) => SizeOf(t, p))
  }

  /* FRONTEND */

  sealed case class Program(fs: List[Func], s: Stat) {
    def printStats(s: Stat, str: String): StringBuilder = {
      val sb = new StringBuilder()
      s match {
        case Seq(stats) =>
          for (k <- stats) {
            sb.append(s"$str $k\n")
          }
        case _ => sb.append(s)
      }
      sb
    }
    override def toString: String = {
      val sb = new StringBuilder()
      for (f <- fs) {
        val Func(t, id, ps, stats) = f
        sb.append(s"FUNCTION $id $ps \n")
        sb.append(printStats(stats, "\t"))
        sb.append(s"END OF FUNCTION\n")
      }
      sb.append("MAIN\n")
      sb.append(printStats(s, ""))
      sb.append("END MAIN\n")
      sb.toString()
    }
  }

  sealed case class Func(
      t: Type,
      id: Ident,
      ps: Option[ParamList] = None,
      s: Stat
  )

  sealed case class ParamList(ps: List[Param]) {
    def map(f: Ident => Ident): ParamList = ParamList(ps.map(p => {
      val Param(t, id) = p
      Param(t, f(id))
    }))
  }

  sealed case class Param(t: Type, id: Ident)

  // Trait for all possible variations of a statement
  sealed trait Stat {
    def map(f: Expr => Expr): Stat = this
  }
  case object Skip extends Stat
  case class EqIdent(t: Type, id: Ident, aRHS: AssignRHS) extends Stat
  case class EqAssign(aLHS: AssignLHS, aRHS: AssignRHS) extends Stat
  case class Read(aLHS: AssignLHS) extends Stat
  case class Free(e: Expr) extends Stat {
    override def map(f: Expr => Expr) = Free(f(e))
  }
  case class Return(e: Expr) extends Stat {
    override def map(f: Expr => Expr) = Return(f(e))
  }
  case class Exit(e: Expr) extends Stat {
    override def map(f: Expr => Expr) = Exit(f(e))
  }
  case class Print(e: Expr) extends Stat {
    override def map(f: Expr => Expr) = Print(f(e))
  }
  case class PrintLn(e: Expr) extends Stat {
    override def map(f: Expr => Expr) = PrintLn(f(e))
  }
  case class If(cond: Expr, s1: Stat, s2: Stat) extends Stat
  case class While(cond: Expr, s: Stat) extends Stat
  case class Begin(s: Stat) extends Stat
  case class Seq(stats: List[Stat]) extends Stat

  sealed trait AssignLHS

  // Trait for all possible variations of a RHS Assignment
  sealed trait AssignRHS {
    val pos: (Int, Int)
    // Abstract function to get type of the RHS
    def getType(sTable: SymbolTable): Type
    // Field to store it's semantic errors
    var semErrs: mutable.ListBuffer[SemanticError] =
      mutable.ListBuffer.empty[SemanticError]
    def map[B >: AssignRHS](f: Expr => Expr): B = this
  }

  case class Newpair(fst: Expr, snd: Expr, pos: (Int, Int)) extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      val fstType = fst.getType(sTable)
      val sndType = snd.getType(sTable)
      semErrs = fst.semErrs ++ snd.semErrs
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
  object Newpair {
    def apply(fst: Parsley[Expr], snd: Parsley[Expr]): Parsley[Newpair] =
      pos <**> (fst, snd).map((fst: Expr, snd: Expr) =>
        (p: (Int, Int)) => Newpair(fst, snd, p)
      )
  }

  case class Call(id: Ident, args: Option[ArgList] = None, pos: (Int, Int))
      extends AssignRHS {
    override def getType(sTable: SymbolTable): Type = {
      val idType = id.getType(sTable)
      semErrs ++= sTable.funcParamMatch(id, args)
      idType
    }
    override def toString: String =
      id.toString + "(" + args.getOrElse(ArgList(List())) + ")"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      Call(id, args.map(arg => arg.map(f)), pos)
  }
  object Call {
    def apply(
        id: Parsley[Ident],
        args: Parsley[Option[ArgList]]
    ): Parsley[Call] =
      pos <**> (id, args).map((id: Ident, args: Option[ArgList]) =>
        (p: (Int, Int)) => Call(id, args, p)
      )
  }

  sealed case class ArgList(args: List[Expr]) {
    override def toString: String = {
      args.mkString(", ")
    }
    def map(f: Expr => Expr) = ArgList(args.map(f))
  }

  sealed trait PairElem extends AssignLHS with AssignRHS {
    val e: Expr
    override def map[B >: AssignRHS](f: Expr => Expr): PairElem = this
  }

  case class Fst(e: Expr, pos: (Int, Int)) extends PairElem {
    override def toString: String = "fst " + e

    override def map[B >: AssignRHS](f: Expr => Expr) = Fst(f(e), pos)

    override def getType(sTable: SymbolTable): Type = {
      e match {
        case Ident(_, _) =>
          val eType = e.getType(sTable)
          eType match {
            case Pair(fst, _) => return fst.getType
            case _            =>
          }
        case _ =>
      }
      semErrs += InvalidPairElem(this)
      Any
    }
  }
  object Fst {
    def apply(e: Parsley[Expr]): Parsley[Fst] =
      pos <**> e.map((e: Expr) => (p: (Int, Int)) => Fst(e, p))
  }

  case class Snd(e: Expr, pos: (Int, Int)) extends PairElem {
    override def toString: String = "snd " + e

    override def map[B >: AssignRHS](f: Expr => Expr) = Snd(f(e), pos)

    override def getType(sTable: SymbolTable): Type = {
      e match {
        case Ident(_, _) =>
          val eType = e.getType(sTable)
          eType match {
            case Pair(_, snd) => return snd.getType
            case _            =>
          }
        case _ =>
      }
      semErrs += InvalidPairElem(this)
      Any
    }
  }
  object Snd {
    def apply(e: Parsley[Expr]): Parsley[Snd] =
      pos <**> e.map((e: Expr) => (p: (Int, Int)) => Snd(e, p))
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
    def isPtr: Boolean = this match {
      case PtrT(_) => true
      case _       => false
    }
  }

  case object Any extends Type {
    override def equals(x: Any): Boolean =
      x match {
        case t: Type => true
        case _       => false
      }
  }

  // All possible Base Types
  sealed trait BaseType extends Type
  case object IntT extends BaseType {
    override def toString: String = "int"
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
    override def toString: String = {
      if (t == null) return "T[]"
      t.toString + "[]"
    }
    override def equals(x: Any): Boolean = x match {
      case ArrayT(null)  => true
      case ArrayT(inner) => if (t == null) true else inner == t
      case _             => false
    }
  }

  sealed trait PairType extends Type
  case class Pair(x: PairElemType, y: PairElemType) extends PairType {
    override def equals(x: Any): Boolean = x match {
      case Pair(_, _) => true
      case _          => false
    }
    override def toString: String = {
      if ((x == null) && (y == null)) {
        return "Pair"
      }
      "pair(" + x + "," + y + ")"
    }
  }

  // Trait for all possible variations of a Pair Elem
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

  // Trait for all possible variations of an expression
  sealed trait Expr extends AssignRHS {
    override def map[B >: AssignRHS](f: Expr => Expr): Expr = this
  }

  // Trait for all possible variations of an unary operation
  sealed trait UnOp extends Expr {
    val e: Expr
    val expected: (Type, Type)
    val unOperatorStr: String

    override def getType(sTable: SymbolTable): Type = {
      val actual = e.getType(sTable)
      semErrs = e.semErrs
      if (actual != expected._1) {
        semErrs += TypeMismatch(e, actual, List(expected._1))
      }
      expected._2
    }

    val containsNoIdent: Boolean = {
      e match {
        case _: Ident | _: ArrayElem => false
        case _                       => true
      }
    }

    override def toString: String = unOperatorStr + e.toString
  }

  case class Not(e: Expr, pos: (Int, Int)) extends UnOp {
    override val expected: (Type, Type) = (BoolT, BoolT)
    val unOperatorStr = "!"
    override def map[B >: AssignRHS](f: Expr => Expr) = Not(f(e), pos)
  }
  object Not {
    def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      pos.map((p: (Int, Int)) => (e: Expr) => Not(e, p)) <* op
  }
  case class Negation(e: Expr, pos: (Int, Int)) extends UnOp {
    override val expected: (Type, Type) = (IntT, IntT)
    val unOperatorStr = "-"
    override def map[B >: AssignRHS](f: Expr => Expr) = Negation(f(e), pos)
  }
  object Negation {
    def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      pos.map((p: (Int, Int)) => (e: Expr) => Negation(e, p)) <* op
  }
  case class Len(e: Expr, pos: (Int, Int)) extends UnOp {
    override val expected: (Type, Type) = (ArrayT(null), IntT)
    val unOperatorStr = "len "
    override def map[B >: AssignRHS](f: Expr => Expr) = Len(f(e), pos)

    override def getType(sTable: SymbolTable): Type = {
      val actual = e.getType(sTable)
      semErrs = e.semErrs
      if (!actual.isArray) {
        semErrs += TypeMismatch(e, actual, List(expected._1))
      }
      expected._2
    }
  }
  object Len {
    def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      pos.map((p: (Int, Int)) => (e: Expr) => Len(e, p)) <* op
  }
  case class Ord(e: Expr, pos: (Int, Int)) extends UnOp {
    override val expected: (Type, Type) = (CharT, IntT)
    val unOperatorStr = "ord "
    override def map[B >: AssignRHS](f: Expr => Expr) = Ord(f(e), pos)
  }
  object Ord {
    def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      pos.map((p: (Int, Int)) => (e: Expr) => Ord(e, p)) <* op
  }
  case class Chr(e: Expr, pos: (Int, Int)) extends UnOp {
    override val expected: (Type, Type) = (IntT, CharT)
    val unOperatorStr = "chr "
    override def map[B >: AssignRHS](f: Expr => Expr) = Chr(f(e), pos)
  }
  object Chr {
    def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      pos.map((p: (Int, Int)) => (e: Expr) => Chr(e, p)) <* op
  }
  case class BitwiseNot(e: Expr, pos: (Int, Int)) extends UnOp {
    override val expected: (Type, Type) = (IntT, IntT)
    val unOperatorStr = "~"
    override def map[B >: AssignRHS](f: Expr => Expr) = BitwiseNot(f(e), pos)
  }
  object BitwiseNot {
    def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      pos.map((p: (Int, Int)) => (e: Expr) => BitwiseNot(e, p)) <* op
  }

  // Trait for all possible variations of an binary operation
  sealed trait BinOp extends Expr {
    val lExpr: Expr
    val rExpr: Expr
    val expected: (List[Type], Type)
    val operatorStr: String

    override def toString: String =
      lExpr.toString + " " + operatorStr + " " + rExpr

    override def getType(sTable: SymbolTable): Type = {
      val actualL = lExpr.getType(sTable)
      val actualR = rExpr.getType(sTable)
      semErrs = lExpr.semErrs ++ rExpr.semErrs
      /* If type is Any, variable used in expression is undefined so early
         return with semantic errors. */
      if (actualL == Any || actualR == Any) {
        return expected._2
      }
      /* If isPtrArithmetic is true, early return with pointer type. */
      if (isPtrArithmetic(actualL, actualR)) {
        return actualL
      }
      if (actualL != actualR) {
        if (!expected._1.contains(actualL)) {
          semErrs += TypeMismatch(lExpr, actualL, expected._1)
        }
        if (!expected._1.contains(actualR)) {
          semErrs += TypeMismatch(rExpr, actualR, expected._1)
        }
      } else if (!expected._1.contains(actualL) && expected._1.nonEmpty) {
        semErrs += TypeMismatch(lExpr, actualL, expected._1)
        semErrs += TypeMismatch(rExpr, actualR, expected._1)
      }
      expected._2
    }

    /* Returns true if pointer arithmetic is occuring, use left type as pointer
       since '+'/'-' is left-associative. */
    def isPtrArithmetic(leftT: Type, rightT: Type): Boolean = this match {
      case Plus(_, _, _) | Sub(_, _, _) => leftT.isPtr && (rightT == IntT)
      case _                            => false
    }

    val containsNoIdent: Boolean = {
      val r = rExpr match {
        case _: Ident | _: ArrayElem => false
        case _                       => true
      }
      lExpr match {
        case op: BinOp               => op.containsNoIdent && r
        case _: Ident | _: ArrayElem => false
        case _                       => true && r
      }
    }
  }

  // Traits for all possible types of an binary operation
  sealed trait ArithOps extends BinOp {
    override val expected: (List[Type], Type) = (List(IntT), IntT)
  }
  case class Mul(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ArithOps {
    val operatorStr = "*"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      Mul(f(lExpr), f(rExpr), pos)
  }
  object Mul {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => Mul(l, r, p)) <* op
  }
  case class Div(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ArithOps {
    val operatorStr = "/"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      Div(f(lExpr), f(rExpr), pos)
  }
  object Div {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => Div(l, r, p)) <* op
  }
  case class Mod(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ArithOps {
    val operatorStr = "%"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      Mod(f(lExpr), f(rExpr), pos)
  }
  object Mod {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => Mod(l, r, p)) <* op
  }
  case class Plus(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ArithOps {
    val operatorStr = "+"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      Plus(f(lExpr), f(rExpr), pos)
  }
  object Plus {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => Plus(l, r, p)) <* op
  }
  case class Sub(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ArithOps {
    val operatorStr = "-"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      Sub(f(lExpr), f(rExpr), pos)
  }
  object Sub {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => Sub(l, r, p)) <* op
  }
  // Traits for comparison operators
  sealed trait ComparOps extends BinOp {
    override val expected: (List[Type], Type) = (List(CharT, IntT), BoolT)
  }
  case class GT(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ComparOps {
    val operatorStr = ">"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      GT(f(lExpr), f(rExpr), pos)
  }
  object GT {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => GT(l, r, p)) <* op
  }
  case class GTE(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ComparOps {
    val operatorStr = ">="
    override def map[B >: AssignRHS](f: Expr => Expr) =
      GTE(f(lExpr), f(rExpr), pos)
  }
  object GTE {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => GTE(l, r, p)) <* op
  }
  case class LT(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ComparOps {
    val operatorStr = "<"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      LT(f(lExpr), f(rExpr), pos)
  }
  object LT {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => LT(l, r, p)) <* op
  }
  case class LTE(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends ComparOps {
    val operatorStr = "<="
    override def map[B >: AssignRHS](f: Expr => Expr) =
      LTE(f(lExpr), f(rExpr), pos)
  }
  object LTE {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => LTE(l, r, p)) <* op
  }
  // Traits for equality operators
  sealed trait EqOps extends BinOp {
    override val expected: (List[Type], Type) = (List.empty, BoolT)
  }
  case class Equal(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends EqOps {
    val operatorStr = "=="
    override def map[B >: AssignRHS](f: Expr => Expr) =
      Equal(f(lExpr), f(rExpr), pos)
  }
  object Equal {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => Equal(l, r, p)) <* op
  }
  case class NotEqual(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends EqOps {
    val operatorStr = "!="
    override def map[B >: AssignRHS](f: Expr => Expr) =
      NotEqual(f(lExpr), f(rExpr), pos)
  }
  object NotEqual {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => NotEqual(l, r, p)) <* op
  }
  // Traits for logical operator
  sealed trait LogicalOps extends BinOp {
    override val expected: (List[Type], Type) = (List(BoolT), BoolT)
  }
  case class And(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends LogicalOps {
    val operatorStr = "&&"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      And(f(lExpr), f(rExpr), pos)
  }
  object And {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => And(l, r, p)) <* op
  }
  case class Or(lExpr: Expr, rExpr: Expr, pos: (Int, Int)) extends LogicalOps {
    val operatorStr = "||"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      Or(f(lExpr), f(rExpr), pos)
  }
  object Or {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => Or(l, r, p)) <* op
  }
  // Traits for bitwise operators
  sealed trait BitwiseOps extends BinOp {
    override val expected: (List[Type], Type) = (List(IntT), IntT)
  }
  case class BitwiseAnd(lExpr: Expr, rExpr: Expr, pos: (Int, Int))
      extends BitwiseOps {
    val operatorStr = "&"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      BitwiseAnd(f(lExpr), f(rExpr), pos)
  }
  object BitwiseAnd {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) =>
        (l: Expr, r: Expr) => BitwiseAnd(l, r, p)
      ) <* op
  }
  case class BitwiseOr(lExpr: Expr, rExpr: Expr, pos: (Int, Int))
      extends BitwiseOps {
    val operatorStr = "|"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      BitwiseOr(f(lExpr), f(rExpr), pos)
  }
  object BitwiseOr {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) => (l: Expr, r: Expr) => BitwiseOr(l, r, p)) <* op
  }
  case class BitwiseXor(lExpr: Expr, rExpr: Expr, pos: (Int, Int))
      extends BitwiseOps {
    val operatorStr = "^"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      BitwiseXor(f(lExpr), f(rExpr), pos)
  }
  object BitwiseXor {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) =>
        (l: Expr, r: Expr) => BitwiseXor(l, r, p)
      ) <* op
  }
  case class LogicalShiftLeft(lExpr: Expr, rExpr: Expr, pos: (Int, Int))
      extends BitwiseOps {
    val operatorStr = "<<"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      LogicalShiftLeft(f(lExpr), f(rExpr), pos)
  }
  object LogicalShiftLeft {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) =>
        (l: Expr, r: Expr) => LogicalShiftLeft(l, r, p)
      ) <* op
  }
  case class LogicalShiftRight(lExpr: Expr, rExpr: Expr, pos: (Int, Int))
      extends BitwiseOps {
    val operatorStr = ">>"
    override def map[B >: AssignRHS](f: Expr => Expr) =
      LogicalShiftRight(f(lExpr), f(rExpr), pos)
  }
  object LogicalShiftRight {
    def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      pos.map((p: (Int, Int)) =>
        (l: Expr, r: Expr) => LogicalShiftRight(l, r, p)
      ) <* op
  }

  sealed case class Ident(s: String, pos: (Int, Int))
      extends AssignLHS
      with AssignRHS
      with Expr {

    override def toString: String = s

    override def getType(sTable: SymbolTable): Type = {
      if (!sTable.contains(this)) {
        semErrs += VariableNotDeclared(this)
        return Any
      }
      sTable.lookupAllType(this)
    }

    override def equals(x: Any): Boolean =
      x match {
        case Ident(s, _) => this.s == s
        case _           => false
      }

    override def hashCode(): Int = s.hashCode()
  }
  object Ident {
    def apply(str: Parsley[String]): Parsley[Ident] =
      pos <**> str.map((s: String) => (p: (Int, Int)) => Ident(s, p))
  }

  sealed case class ArrayElem(id: Ident, exprs: List[Expr], pos: (Int, Int))
      extends AssignLHS
      with Expr {

    override def getType(sTable: SymbolTable): Type = {
      val actual = id.getType(sTable)
      if (actual.isArray || actual.isPtr) {
        actual match {
          case PtrT(t)   => return t
          case ArrayT(t) => return t
          case _         => ???
        }
      }
      id.semErrs += TypeMismatch(id, actual, List(ArrayT(actual)))
      id.semErrs += ElementAccessDenied(id)
      actual
    }
  }
  object ArrayElem {
    def apply(id: Parsley[Ident], es: Parsley[List[Expr]]): Parsley[ArrayElem] =
      pos <**> (id, es).map((id: Ident, es: List[Expr]) =>
        (p: (Int, Int)) => ArrayElem(id, es, p)
      )
  }

  sealed trait Liter extends Expr

  sealed case class IntLiter(n: Int, pos: (Int, Int)) extends Liter {
    override def toString: String = n.toString
    override def getType(sTable: SymbolTable): Type = IntT
    override def equals(x: Any): Boolean =
      x match {
        case IntLiter(n, _) => this.n == n
        case _              => false
      }
  }
  object IntLiter {
    def apply(n: Parsley[Int]): Parsley[IntLiter] =
      pos <**> n.map((n: Int) => (p: (Int, Int)) => IntLiter(n, p))
  }

  // Trait for an sign checking for integer representation
  sealed trait IntSign
  case object Pos extends IntSign
  case object Neg extends IntSign

  sealed case class BoolLiter(b: Boolean, pos: (Int, Int)) extends Liter {
    override def toString: String = b.toString
    override def getType(sTable: SymbolTable): Type = BoolT
    override def equals(x: Any): Boolean =
      x match {
        case BoolLiter(b, _) => this.b == b
        case _               => false
      }
  }
  object BoolLiter {
    def apply(b: Boolean): Parsley[BoolLiter] =
      pos.map((p: (Int, Int)) => BoolLiter(b, p))
  }

  sealed case class CharLiter(c: Character, pos: (Int, Int)) extends Liter {
    override def toString: String = "'" + c.toString + "'"
    override def getType(sTable: SymbolTable): Type = CharT
    override def equals(x: Any): Boolean =
      x match {
        case CharLiter(c, _) => this.c == c
        case _               => false
      }
  }
  object CharLiter {
    def apply(c: Parsley[Character]): Parsley[CharLiter] =
      pos <**> c.map((c: Character) => (p: (Int, Int)) => CharLiter(c, p))
  }

  // Trait for different types of characters
  sealed trait Character
  case class NormalChar(c: Char) extends Character {
    override def toString: String = c.toString
  }
  case class Escape(c: Char) extends Character {
    override def toString: String = s"\\$c"
  }

  sealed case class StrLiter(str: List[Character], pos: (Int, Int))
      extends Liter {
    override def toString: String = str.mkString("")
    override def getType(sTable: SymbolTable): Type = StringT
    override def equals(x: Any): Boolean =
      x match {
        case StrLiter(str, _) => this.str == str
        case _                => false
      }
  }
  object StrLiter {
    def apply(str: Parsley[List[Character]]): Parsley[StrLiter] =
      pos <**> str.map((str: List[Character]) =>
        (p: (Int, Int)) => StrLiter(str, p)
      )
  }

  sealed case class ArrayLiter(arr: Option[List[Expr]], pos: (Int, Int))
      extends AssignRHS {
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
    def len(): Int = {
      arr match {
        case Some(es) => es.size
        // empty
        case None => 0
      }
    }
  }
  object ArrayLiter {
    def apply(exprs: Parsley[Option[List[Expr]]]): Parsley[ArrayLiter] =
      pos <**> exprs.map((exprs: Option[List[Expr]]) =>
        (p: (Int, Int)) => ArrayLiter(exprs, p)
      )
  }

  sealed case class PairLiter(pos: (Int, Int)) extends Liter {
    override def toString: String = "null"
    override def getType(sTable: SymbolTable): Type =
      Pair(null, null)
  }
  object PairLiter {
    def apply(): Parsley[PairLiter] =
      pos.map((p: (Int, Int)) => PairLiter(p))
  }
}
