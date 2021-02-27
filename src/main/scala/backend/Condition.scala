package backend

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
  override val oppositeCmp: Condition = GTE
}
case object LTE extends Condition {
  override val oppositeCmp: Condition = GT
}
case object GT extends Condition {
  override val oppositeCmp: Condition = LTE
}
case object GTE extends Condition {
  override val oppositeCmp: Condition = LT
}
