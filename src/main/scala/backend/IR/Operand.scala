package backend.IR

import backend.IR.InstructionSet.Label
import frontend.Rules.Character

object Operand {
  sealed trait Operand
  sealed case class ImmInt(n: Int) extends Operand {
    override def toString: String = s"#$n"
  }

  case class LSL(r: Reg, n: ImmInt) extends Operand {
    override def toString: String = s"${r.toString}, LSL $n"
  }

  case class ASR(r: Reg, n: ImmInt) extends Operand {
    override def toString: String = s"${r.toString}, ASR $n"
  }

  sealed case class ImmChar(c: Character) extends Operand with LoadOperand {
    override def toString: String = s"#'$c'"
  }

  sealed trait Address extends LoadOperand
  sealed case class RegisterOffset(r: Reg, n: Int) extends Address {
    override def toString: String = s"[$r, #$n]"
  }

  sealed case class RegAdd(r: Reg) extends Address {
    override def toString: String = s"[$r]"
  }

  sealed trait LoadOperand
  sealed case class ImmMem(n: Int) extends LoadOperand {
    override def toString: String = s"=$n"
  }

  sealed case class DataLabel(label: Label) extends Operand with LoadOperand {
    override def toString: String = s"=$label"
  }

  sealed trait Reg extends Operand
  case object R0 extends Reg {
    override def toString: String = "r0"
  }
  case object R1 extends Reg {
    override def toString: String = "r1"
  }
  case object R2 extends Reg {
    override def toString: String = "r2"
  }
  case object R3 extends Reg {
    override def toString: String = "r3"
  }
  case object R4 extends Reg {
    override def toString: String = "r4"
  }
  case object R5 extends Reg {
    override def toString: String = "r5"
  }
  case object R6 extends Reg {
    override def toString: String = "r6"
  }
  case object R7 extends Reg {
    override def toString: String = "r7"
  }
  case object R8 extends Reg {
    override def toString: String = "r8"
  }
  case object R9 extends Reg {
    override def toString: String = "r9"
  }
  case object R10 extends Reg {
    override def toString: String = "r10"
  }
  case object R11 extends Reg {
    override def toString: String = "r11"
  }

  // Stack Pointer
  case object SP extends Reg {
    override def toString: String = "sp"
  }

  // Link Register
  case object LR extends Reg {
    override def toString: String = "lr"
  }

  // Program Counter
  case object PC extends Reg {
    override def toString: String = "pc"
  }

}
