package backend.IR

import scala.collection.mutable.ListBuffer
import backend.IR.Operand._
import backend.IR.Condition._

object InstructionSet {

  sealed trait Instruction

  /* Arithmetic operations */
  case class Add(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"ADD $rd, $rn, $op2"
  }
  case class AddS(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"ADDS $rd, $rn, $op2"
  }
  case class Sub(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"SUB $rd, $rn, $op2"
  }
  case class SubS(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"SUBS $rd, $rn, $op2"
  }
  case class SMul(rdLo: Reg, rdHi: Reg, rn: Reg, rm: Reg) extends Instruction {
    override def toString: String = s"SMULL $rdLo, $rdHi, $rn, $rm"
  }
  case class RsbS(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"RSBS $rd, $rn, $op2"
  }

  /* Comparison */
  case class Cmp(rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"CMP $rn , $op2"
  }

  /* Logical operations */
  case class And(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"AND $rd, $rn, $op2"
  }
  case class Or(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"ORR $rd, $rn, $op2"
  }
  case class Eor(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"EOR $rd, $rn, $op2"
  }

  /* Branching */
  case class Branch(label: Label) extends Instruction {
    override def toString: String = s"B $label"
  }
  // Branch {Condition}
  case class BranchCond(cond: Condition, label: Label) extends Instruction {
    override def toString: String = s"B$cond $label"
  }
  // Branch Link
  case class BranchLink(label: Label) extends Instruction {
    override def toString: String = s"BL $label"
  }
  // Branch Link {Condition}
  case class BranchLinkCond(cond: Condition, label: Label) extends Instruction {
    override def toString: String = s"BL$cond $label"
  }

  /* Stack Operations */
  case class Push(rs: ListBuffer[Reg]) extends Instruction {
    val regs = rs.mkString(", ")
    override def toString: String = s"PUSH {$regs}"
  }
  case class Pop(rs: ListBuffer[Reg]) extends Instruction {
    val regs = rs.mkString(", ")
    override def toString: String = s"POP {$regs}"
  }

  /* Move Operations */
  case class Mov(rd: Reg, op2: Operand) extends Instruction {
    override def toString: String = s"MOV $rd, $op2"
  }
  // Move {Condition}
  case class MovCond(cond: Condition, rd: Reg, op2: Operand)
      extends Instruction {
    override def toString: String = s"MOV$cond $rd, $op2"
  }

  /* Loading */
  case class Ldr(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = s"LDR $rd, $op2"
  }
  object Ldr {
    def apply(isByte: Boolean, src: Reg, dst: Reg, offset: Int): Instruction = {
      if (isByte) LdrSB.apply(src, dst, offset) else apply(src, dst, offset)
    }
    def apply(src: Reg, dst: Reg, offset: Int): Instruction = {
      if (offset == 0) Ldr(src, RegAdd(dst))
      else Ldr(src, RegisterOffset(dst, offset))
    }
  }
  // Load byte signed
  case class LdrSB(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = s"LDRSB $rd, $op2"
  }
  object LdrSB {
    def apply(src: Reg, dst: Reg, offset: Int): Instruction = {
      if (offset == 0) LdrSB(src, RegAdd(dst))
      else LdrSB(src, RegisterOffset(dst, offset))
    }
  }
  // Load {Condition}
  case class LdrCond(cond: Condition, rd: Reg, op2: LoadOperand)
      extends Instruction {
    override def toString: String = s"LDR $cond $rd, $op2"
  }

  /* Storing */
  case class Str(rd: Reg, add: Address) extends Instruction {
    override def toString: String = s"STR $rd, $add"
  }
  object Str {
    def apply(isByte: Boolean, src: Reg, dst: Reg, offset: Int): Instruction = {
      if (isByte) StrB.apply(src, dst, offset) else apply(src, dst, offset)
    }
    def apply(src: Reg, dst: Reg, offset: Int): Instruction = {
      if (offset == 0) Str(src, RegAdd(dst))
      else Str(src, RegisterOffset(dst, offset))
    }
  }
  // Store byte
  case class StrB(rd: Reg, add: Address) extends Instruction {
    override def toString: String = s"STRB $rd, $add"
  }
  object StrB {
    def apply(src: Reg, dst: Reg, offset: Int): Instruction = {
      if (offset == 0) StrB(src, RegAdd(dst))
      else StrB(src, RegisterOffset(dst, offset))
    }
  }
  case class StrOffsetIndex(rd: Reg, regAdd: Reg, offset: Int)
      extends Instruction {
    override def toString: String =
      s"STR $rd, [$regAdd, #$offset]!"
  }
  object StrOffsetIndex {
    def apply(isByte: Boolean, src: Reg, dst: Reg, offset: Int): Instruction = {
      if (isByte) StrBOffsetIndex(src, dst, offset)
      else StrOffsetIndex(src, dst, offset)
    }
  }
  case class StrBOffsetIndex(rd: Reg, regAdd: Reg, offset: Int)
      extends Instruction {
    override def toString: String =
      s"STRB $rd, [$regAdd, #$offset]!"
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
