package backend
import Rules._

sealed trait Operand
sealed case class ImmInt(n: Int) extends Operand
sealed case class ImmChar(c: Character) extends Operand with LoadOperand
sealed case class DataLabel(label: String) extends Operand with LoadOperand

sealed trait Address
sealed case class Offset(n: Int) extends Address

sealed trait LoadOperand
sealed case class ImmMem(n: Int) extends LoadOperand

sealed trait Reg extends Operand with LoadOperand with Address
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

// Stack Pointer
case object SP extends Reg

// Link Register
case object LR extends Reg

// Program Counter
case object PC extends Reg
