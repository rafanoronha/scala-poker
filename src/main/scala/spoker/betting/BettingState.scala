package spoker.betting

import spoker.ManageablePlayer

case class BettingState(
  currentBet: Option[BettingAction] = None,
  placedBy: Better = null,
  bettersToAct: Seq[ManageablePlayer],
  bettersActed: Seq[ManageablePlayer] = Nil,
  matchedBy: Seq[ManageablePlayer] = Nil)
