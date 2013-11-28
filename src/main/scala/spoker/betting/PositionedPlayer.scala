package spoker.betting

import spoker.Player
import spoker.betting.stack.StackHolder

class PositionedPlayer(
  p: Player,
  table: Table,
  val position: Position.Value = Position.Any,
  val stack: Int = 50) extends StackHolder {

  def copy(
    p: Player = this.p,
    table: Table = this.table,
    position: Position.Value = this.position,
    stack: Int = this.stack) =
    new PositionedPlayer(
      p = p,
      table = table,
      position = position,
      stack = stack)

  val chief = table

  val playerName = p.name

  def player = table.players.find(playerName == _.name).get

  override def equals(that: Any) = that match {
    case pp: PositionedPlayer => this.player == pp.player
  }

  override def hashCode = this.player.##

}
