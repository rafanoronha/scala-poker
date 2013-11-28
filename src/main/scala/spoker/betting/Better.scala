package spoker.betting

import spoker.Player

case class Better(positionedPlayer: PositionedPlayer) {

  lazy val myAction = BetterAction(this)

  def myActionIs(a: Action) = myAction(a)

  def call = myActionIs(Call)

  def check = myActionIs(Check)

  def raise(value: Int) = myActionIs(Raise(value))

  def fold = myActionIs(Fold)

  override def equals(that: Any) = that match {
    case b: Better => playerFromBetter(this) == playerFromBetter(b)
    case pp: PositionedPlayer => playerFromBetter(this) == pp.player
    case p: Player => playerFromBetter(this) == p
  }

  override def hashCode = playerFromBetter(this).##
}
