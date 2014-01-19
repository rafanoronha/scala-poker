package spoker.betting

import spoker.Player

case class PositionedPlayer(
  player: Player,
  initialStack: Double,
  isButton: Boolean = false,
  isActive: Boolean = true,
  isAllIn: Boolean = false) {

  val name = player.name

  override def hashCode = this.player.##
}
