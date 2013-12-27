package spoker.betting

import spoker.ManageablePlayer

case class Bet(
  value: Int = 0,
  placedBy: Better = null,
  bettersToAct: Iterator[ManageablePlayer],
  matchedBy: Seq[ManageablePlayer] = Nil)
