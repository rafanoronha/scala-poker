package spoker.dealer

sealed trait DealtCardTarget

object Community extends DealtCardTarget

object Burn extends DealtCardTarget

case class PlayerReceivingCard(playerName: String) extends DealtCardTarget
