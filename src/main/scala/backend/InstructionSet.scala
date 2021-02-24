package backend

import scala.collection.mutable.ListBuffer

package object InstructionSet {

  sealed trait Instruction

  // arithmetic
  case class Add(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "ADD " + rd + ", " + rn + ", " + op2
  }
  case class Sub(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "SUB " + rd + ", " + rn + ", " + op2
  }
  case class Mul(rd: Reg, rm: Reg, rs: Reg) extends Instruction {
    override def toString: String = "MUL " + rd + ", " + rm + ", " + rs
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
    override def toString: String = "OR " + rd + ", " + rn + ", " + op2
  }

  // Branching
  case class Branch(label: Label) extends Instruction {
    override def toString: String = "B " + label
  }
  case class BranchLink(label: Label) extends Instruction {
    override def toString: String = "BL " + label
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

  case class StrOffset(rd: Reg, regAdd: Reg, offset: Int) extends Instruction {
    override def toString: String =
      "STR " + rd + ", " + "[" + regAdd + ", #" + offset + "]"
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
    override def toString: String = "STRB"
  }

  case class Label(s: String) {
    override def toString: String = s
  }

  case class Data(label: Label, s: String)
}
