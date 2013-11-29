package spoker.betting.stack

import spoker.betting.Table

case class Pot(
  blinds: Blinds,
  tableName: String) extends BlindsGathering {

  val name = tableName
}
