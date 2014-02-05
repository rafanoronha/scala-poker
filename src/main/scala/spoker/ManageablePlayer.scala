package spoker

import spoker.betting.PositionedPlayer
import spoker.dealer.CardsManagement

case class ManageablePlayer(
  val positionedPlayer: PositionedPlayer,
  val cardsManagement: CardsManagement ) {

  val name = positionedPlayer.name

  def isButton = positionedPlayer.isButton

  def cards: Cards = cardsManagement.currentState(name)

  def folded = copy(
    positionedPlayer = positionedPlayer.copy(status = PlayerStatus.Folded))

  def pushedAllIn = copy(
    positionedPlayer = positionedPlayer.copy(status = PlayerStatus.AllIn))

  override def equals(that: Any) = that match {
    case mp: ManageablePlayer => this.positionedPlayer == mp.positionedPlayer
  }
}