package spoker.betting

import spoker.ManageablePlayer

case class BettingState(
  bettersToAct: Seq[ManageablePlayer],
  bettersActed: Seq[ManageablePlayer] = Nil) {
}
