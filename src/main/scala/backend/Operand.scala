package backend

sealed trait Operand

sealed class Imm(n: Int) extends Operand

sealed trait Reg extends Operand
case object R0 extends Reg
case object R1 extends Reg
case object R2 extends Reg
case object R3 extends Reg
case object R4 extends Reg
case object R5 extends Reg
case object R6 extends Reg
case object R7 extends Reg
case object R8 extends Reg
case object R9 extends Reg
case object R10 extends Reg
case object R11 extends Reg
case object R12 extends Reg
case object R13 extends Reg
case object R14 extends Reg

// TODO: Stack Pointer, Instruction Register, Program Counter
