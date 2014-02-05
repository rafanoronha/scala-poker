package spoker.betting

sealed trait Action

trait BettingAction {
  val value: Int
}

object Call extends Action

object Check extends Action

object Fold extends Action

object AllIn extends Action

case class Raise(value: Int) extends Action with BettingAction

case class Bet(value: Int) extends Action with BettingAction
