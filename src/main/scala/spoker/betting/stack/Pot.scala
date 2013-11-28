package spoker.betting.stack

import spoker.betting.Table

case class Pot(
  stack: Int = 0,
  blinds: Blinds,
  var table: Table = null) extends BlindsGathering {
  def withTable(t: Table): Pot = {
    table = t
    this
  }

  lazy val chief = table
}
