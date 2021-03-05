package backend.IR

import scala.collection.mutable.ListBuffer
import backend.IR.Operand._
import backend.IR.Condition._

object InstructionSet {

  sealed trait Instruction

  /* Arithmetic operations */
  case class Add(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "ADD " + rd + ", " + rn + ", " + op2
  }
  case class AddS(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "ADDS " + rd + ", " + rn + ", " + op2
  }
  case class Sub(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "SUB " + rd + ", " + rn + ", " + op2
  }
  case class SubS(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "SUBS " + rd + ", " + rn + ", " + op2
  }
  case class SMul(rdLo: Reg, rdHi: Reg, rn: Reg, rm: Reg) extends Instruction {
    override def toString: String =
      "SMULL " + rdLo + ", " + rdHi + ", " + rn + ", " + rm
  }
  case class RsbS(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "RSBS " + rd + ", " + rn + ", " + op2
  }

  /* Comparison */
  case class Cmp(rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "CMP " + rn + ", " + op2
  }

  /* Logical operations */
  case class And(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "AND " + rd + ", " + rn + ", " + op2
  }
  case class Or(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "ORR " + rd + ", " + rn + ", " + op2
  }
  case class Eor(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "EOR " + rd + ", " + rn + ", " + op2
  }

  /* Branching */
  case class Branch(label: Label) extends Instruction {
    override def toString: String = "B " + label
  }
  // Branch {Condition}
  case class BranchCond(cond: Condition, label: Label) extends Instruction {
    override def toString: String = "B" + cond + " " + label
  }
  // Branch Link
  case class BranchLink(label: Label) extends Instruction {
    override def toString: String = "BL " + label
  }
  // Branch Link {Condition}
  case class BranchLinkCond(cond: Condition, label: Label) extends Instruction {
    override def toString: String = "BL" + cond + " " + label
  }

  /* Stack Operations */
  case class Push(rs: ListBuffer[Reg]) extends Instruction {
    override def toString: String = "PUSH " + "{" + rs.mkString(", ") + "}"
  }
  case class Pop(rs: ListBuffer[Reg]) extends Instruction {
    override def toString: String = "POP " + "{" + rs.mkString(", ") + "}"
  }

  /* Move Operations */
  case class Mov(rd: Reg, op2: Operand) extends Instruction {
    override def toString: String = "MOV " + rd + ", " + op2
  }
  // Move {Condition}
  case class MovCond(cond: Condition, rd: Reg, op2: Operand)
      extends Instruction {
    override def toString: String = "MOV" + cond + " " + rd + ", " + op2
  }

  /* Loading */
  case class Ldr(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = "LDR " + rd + ", " + op2
  }
  object Ldr {
    def apply(isByte: Boolean, src: Reg, dst: Reg, offset: Int): Instruction = {
      if (isByte) {
        return LdrSB.apply(src, dst, offset)
      }
      apply(src, dst, offset)
    }
    def apply(src: Reg, dst: Reg, offset: Int): Instruction = {
      if (offset == 0) {
        return Ldr(src, RegAdd(dst))
      }
      Ldr(src, RegisterOffset(dst, offset))
    }
  }
  // Load byte signed
  case class LdrSB(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = "LDRSB " + rd + ", " + op2
  }
  object LdrSB {
    def apply(src: Reg, dst: Reg, offset: Int): Instruction = {
      if (offset == 0) {
        return LdrSB(src, RegAdd(dst))
      }
      LdrSB(src, RegisterOffset(dst, offset))
    }
  }
  // Load {Condition}
  case class LdrCond(cond: Condition, rd: Reg, op2: LoadOperand)
      extends Instruction {
    override def toString: String = "LDR" + cond + " " + rd + ", " + op2
  }

  /* Storing */
  case class Str(rd: Reg, add: Address) extends Instruction {
    override def toString: String = "STR " + rd + ", " + add
  }
  object Str {
    def apply(isByte: Boolean, src: Reg, dst: Reg, offset: Int): Instruction = {
      if (isByte) {
        return StrB.apply(src, dst, offset)
      }
      apply(src, dst, offset)
    }
    def apply(src: Reg, dst: Reg, offset: Int): Instruction = {
      if (offset == 0) {
        return Str(src, RegAdd(dst))
      }
      Str(src, RegisterOffset(dst, offset))
    }
  }
  // Store byte
  case class StrB(rd: Reg, add: Address) extends Instruction {
    override def toString: String = "STRB " + rd + ", " + add
  }
  object StrB {
    def apply(src: Reg, dst: Reg, offset: Int): Instruction = {
      if (offset == 0) {
        return StrB(src, RegAdd(dst))
      }
      StrB(src, RegisterOffset(dst, offset))
    }
  }
  case class StrOffsetIndex(rd: Reg, regAdd: Reg, offset: Int)
      extends Instruction {
    override def toString: String =
      "STR " + rd + ", " + "[" + regAdd + ", #" + offset + "]!"
  }
  object StrOffsetIndex {
    def apply(isByte: Boolean, src: Reg, dst: Reg, offset: Int): Instruction = {
      if (isByte) {
        return StrBOffsetIndex(src, dst, offset)
      }
      StrOffsetIndex(src, dst, offset)
    }
  }
  case class StrBOffsetIndex(rd: Reg, regAdd: Reg, offset: Int)
      extends Instruction {
    override def toString: String =
      "STRB " + rd + ", " + "[" + regAdd + ", #" + offset + "]!"
  }

  case object Ltorg extends Instruction {
    override def toString: String = ".ltorg"
  }

  /* Labels */
  case class Label(s: String) {
    override def toString: String = s
  }
  case class Data(label: Label, s: String)

}
