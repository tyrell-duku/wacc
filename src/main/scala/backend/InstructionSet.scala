package backend

import scala.collection.mutable.ListBuffer

package object InstructionSet {

  sealed trait Instruction

  // arithmetic
  case class Add(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "ADD " + rd + ", " + rn + ", " + op2
  }
  case class AddS(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "ADDS " + rd + ", " + rn + ", " + op2
  }
  case class Sub(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "SUB " + rd + ", " + rn + ", " + op2
  }
  case class Mul(rd: Reg, rm: Reg, rs: Reg) extends Instruction {
    override def toString: String = "MUL " + rd + ", " + rm + ", " + rs
  }
  case class SMul(rdLo: Reg, rdHi: Reg, rn: Reg, rm: Reg) extends Instruction {
    override def toString: String =
      "SMULL " + rdLo + ", " + rdHi + ", " + rn + ", " + rm
  }
  case class NegInstr(rd: Reg, rm: Reg) extends Instruction {
    override def toString: String = "NEG " + rd + ", " + rm
  }

  // comparison
  case class Cmp(rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "CMP " + rn + ", " + op2
  }

  // logical
  case class And(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "AND " + rd + ", " + rn + ", " + op2
  }
  case class Or(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "ORR " + rd + ", " + rn + ", " + op2
  }
  case class Eor(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "EOR " + rd + ", " + rn + ", " + op2
  }

  // Branching
  case class Branch(label: Label) extends Instruction {
    override def toString: String = "B " + label
  }
  case class BranchLink(label: Label) extends Instruction {
    override def toString: String = "BL " + label
  }
  // Branch Link Not Equal
  case class BranchLinkNE(label: Label) extends Instruction {
    override def toString: String = "BLNE " + label
  }
  // Branch Link Overflow
  case class BranchLinkVS(label: Label) extends Instruction {
    override def toString: String = "BLVS " + label
  }
  // Branch Link Less Than
  case class BranchLinkLT(label: Label) extends Instruction {
    override def toString: String = "BLLT " + label
  }
  // Branch Link Equal
  case class BranchLinkEQ(label: Label) extends Instruction {
    override def toString: String = "BLEQ " + label
  }
  case class BranchEq(label: Label) extends Instruction {
    override def toString: String = "BEQ " + label
  }
  // Branch Link Carry Set
  case class BranchLinkCS(label: Label) extends Instruction {
    override def toString: String = "BLCS " + label
  }

  // Creating labels
  case class Push(rs: ListBuffer[Reg]) extends Instruction {
    override def toString: String = "PUSH " + "{" + rs.mkString(", ") + "}"
  }
  case class Pop(rs: ListBuffer[Reg]) extends Instruction {
    override def toString: String = "POP " + "{" + rs.mkString(", ") + "}"
  }
  case class Ldr(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = "LDR " + rd + ", " + op2
  }
  case class LdrB(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = "LDRB " + rd + ", " + op2
  }
  // LDR Equal
  case class LdrEQ(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = "LDREQ " + rd + ", " + op2
  }
  // LDR Less Than
  case class LdrLT(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = "LDRLT " + rd + ", " + op2
  }
  // LDR Carry Set
  case class LdrCS(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = "LDRCS " + rd + ", " + op2
  }
  case class LdrOffset(rd: Reg, regAdd: Reg, offset: Int) extends Instruction {
    override def toString: String =
      "LDR " + rd + ", " + "[" + regAdd + ", #" + offset + "]"
  }
  case class LdrSB(rd: Reg, op2: LoadOperand) extends Instruction {
    override def toString: String = "LDRSB " + rd + ", " + op2
  }

  case class LdrCond(cond: Condition, rd: Reg, op2: LoadOperand)
      extends Instruction {
    override def toString: String = "LDR" + cond + " " + rd + ", " + op2
  }

  case class Mov(rd: Reg, op2: Operand) extends Instruction {
    override def toString: String = "MOV " + rd + ", " + op2
  }

  case class MovCond(cond: Condition, rd: Reg, op2: Operand)
      extends Instruction {
    override def toString: String = "MOV" + cond + " " + rd + ", " + op2
  }

  // Branching
  case class Str(rd: Reg, add: Address) extends Instruction {
    override def toString: String = "STR " + rd + ", " + add
  }

  case class StrB(rd: Reg, add: Address) extends Instruction {
    override def toString: String = "STRB " + rd + ", " + add
  }

  case class StrOffset(rd: Reg, regAdd: Reg, offset: Int) extends Instruction {
    override def toString: String =
      "STR " + rd + ", " + "[" + regAdd + ", #" + offset + "]"
  }

  case class StrOffsetIndex(rd: Reg, regAdd: Reg, offset: Int)
      extends Instruction {
    override def toString: String =
      "STR " + rd + ", " + "[" + regAdd + ", #" + offset + "]!"
  }

  case class StrBOffsetIndex(rd: Reg, regAdd: Reg, offset: Int)
      extends Instruction {
    override def toString: String =
      "STRB " + rd + ", " + "[" + regAdd + ", #" + offset + "]!"
  }

  case object Ltorg extends Instruction {
    override def toString: String = ".ltorg"
  }

  case class Label(s: String) {
    override def toString: String = s
  }

  case class Data(label: Label, s: String)
}
