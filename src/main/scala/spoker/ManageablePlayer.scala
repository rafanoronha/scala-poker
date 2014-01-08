package spoker

import spoker.betting.PositionedPlayer
import spoker.betting.stack.{StackHolder, StackManagement}
import spoker.dealer.CardsManagement

case class ManageablePlayer(
  val positionedPlayer: PositionedPlayer,
  val cardsManagement: CardsManagement,
  val stackManagement: StackManagement) extends StackHolder {

  val name = positionedPlayer.name

  def isButton = positionedPlayer.isButton

  def cards: Cards = cardsManagement.currentState(name)

  def folded = copy(
    positionedPlayer = positionedPlayer.copy(isActive = false))

  override def equals(that: Any) = that match {
    case mp: ManageablePlayer => this.player == mp.player
  }
}