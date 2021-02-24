package backend

trait Condition
case object EQ extends Condition
case object NE extends Condition
case object LT extends Condition
case object LTE extends Condition
case object GT extends Condition
case object GTE extends Condition
