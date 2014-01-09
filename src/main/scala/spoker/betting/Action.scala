package spoker.betting

sealed trait Action

object Call extends Action

object Check extends Action

object Fold extends Action

case class Raise(value: Int) extends Action

case class Bet(value: Int) extends Action
