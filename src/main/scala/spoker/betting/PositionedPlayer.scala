package spoker.betting

import spoker.{ Player, Cards }
import spoker.betting.stack.StackHolder
import spoker.dealer.CardsManagement

case class PositionedPlayer(
  player: Player,
  tableName: String,
  position: Position.Value = Position.Any) extends StackHolder {

  val name = player.name

  def cards: Cards = CardsManagement.currentState(tableName, name)

  override def equals(that: Any) = that match {
    case pp: PositionedPlayer => this.player == pp.player
  }

  override def hashCode = this.player.##
}
