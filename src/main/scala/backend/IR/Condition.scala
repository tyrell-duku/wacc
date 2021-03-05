package backend.IR

object Condition {
  sealed trait Condition {
    val oppositeCmp: Condition = null
  }
  // Equal
  case object EQ extends Condition {
    override val oppositeCmp: Condition = NE
  }
  // Not equal
  case object NE extends Condition {
    override val oppositeCmp: Condition = EQ
  }
  // Signed less than
  case object LT extends Condition {
    override val oppositeCmp: Condition = GE
  }
  // Signed less than or equal
  case object LE extends Condition {
    override val oppositeCmp: Condition = GT
  }
  // Signed greater than
  case object GT extends Condition {
    override val oppositeCmp: Condition = LE
  }
  // Signed greater or equal
  case object GE extends Condition {
    override val oppositeCmp: Condition = LT
  }
  // Overflow
  case object VS extends Condition
  // Carry set
  case object CS extends Condition
}
