package spoker.betting

import spoker.Player
import spoker.PlayerStatus._

case class PositionedPlayer(
  player: Player,
  initialStack: Int,
  isButton: Boolean = false,
  status: Status = Active) {

  val name = player.name

  override def hashCode = this.player.##
}
