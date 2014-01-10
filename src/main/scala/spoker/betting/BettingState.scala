package spoker.betting

import spoker.ManageablePlayer

case class BettingState(
  currentBet: Option[BettingAction] = None,
  placedBy: Better = null,
  bettersToAct: Iterator[ManageablePlayer],
  matchedBy: Seq[ManageablePlayer] = Nil)
