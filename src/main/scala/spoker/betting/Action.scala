package spoker.betting

sealed trait Action

trait BettingAction {
  val value: Double
}

object Call extends Action

object Check extends Action

object Fold extends Action

object AllIn extends Action

case class Raise(value: Double) extends Action with BettingAction

case class Bet(value: Double) extends Action with BettingAction
