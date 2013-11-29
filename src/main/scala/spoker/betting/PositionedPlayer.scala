package spoker.betting

import spoker.Player
import spoker.betting.stack.StackHolder

case class PositionedPlayer(
  player: Player,
  tableName: String,
  position: Position.Value = Position.Any) extends StackHolder {

  val name = player.name

  override def equals(that: Any) = that match {
    case pp: PositionedPlayer => this.player == pp.player
  }

  override def hashCode = this.player.##
}
