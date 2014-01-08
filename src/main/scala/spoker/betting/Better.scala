package spoker.betting

import spoker.{Player, ManageablePlayer}

case class Better(manageablePlayer: ManageablePlayer) {

  lazy val myAction = BetterAction(this)

  def myActionIs(a: Action) = myAction(a)

  def call = myActionIs(Call)

  def check = myActionIs(Check)

  def raise(value: Int) = myActionIs(Raise(value))

  def fold = myActionIs(Fold)

  override def equals(that: Any) = that match {
    case b: Better => playerFromBetter(this) == playerFromBetter(b)
    case mp: ManageablePlayer => playerFromBetter(this) == mp.positionedPlayer.player
  }

  override def hashCode = playerFromBetter(this).##
}
