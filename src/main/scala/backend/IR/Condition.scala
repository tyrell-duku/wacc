package backend.IR

object Condition {
  sealed trait Condition {
    val oppositeCmp: Condition
  }

  case object EQ extends Condition {
    override val oppositeCmp: Condition = NE
  }
  case object NE extends Condition {
    override val oppositeCmp: Condition = EQ
  }
  case object LT extends Condition {
    override val oppositeCmp: Condition = GE
  }
  case object LE extends Condition {
    override val oppositeCmp: Condition = GT
  }
  case object GT extends Condition {
    override val oppositeCmp: Condition = LE
  }
  case object GE extends Condition {
    override val oppositeCmp: Condition = LT
  }
}
