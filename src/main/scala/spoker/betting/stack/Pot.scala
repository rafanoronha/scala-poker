package spoker.betting.stack

import spoker.betting.Table

case class Pot(
  blinds: Blinds,
  tableName: String) extends AnyRef with BlindsGathering with PotStackManagement {

  val name = tableName
  
  // FIXME: discontinue this start/stop stuff
  val hack = List("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8")
  startManaging(name, hack)

  def collectUntil(stack: Int)(from: StackHolder): Unit = {
    val value = stack - potStackByPlayer(name, from.name)
    collect(value)(from)
  }

  override def collect(stack: Int)(from: StackHolder): Unit = {
    super.collect(stack)(from)
    potStackCollected(name, from.name, stack)
  }
}
