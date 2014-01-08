package spoker.betting

import spoker.Player

case class PositionedPlayer(
  player: Player,
  isButton: Boolean = false,
  isActive: Boolean = true) {

  val name = player.name

  override def hashCode = this.player.##
}
