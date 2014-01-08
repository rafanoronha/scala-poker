package spoker.betting.stack

case class Pot(
  blinds: Blinds,
  stackManagement: StackManagement) extends AnyRef with BlindsGathering {
  
  val name = "Pot"

  val bettingStackManagement = new StackManagement {
    def initialState(holderName: String): Int = 0
  }

  def collectUntil(stack: Int)(from: StackHolder): Unit = {
    val value = stack - potStackByPlayer(from.name)
    collect(value)(from)
  }

  override def collect(stack: Int)(from: StackHolder): Unit = {
    super.collect(stack)(from)
    potStackCollected(from.name, stack)
  }

  private def potStackByPlayer(name: String): Int =
    bettingStackManagement.currentState(name)

  private def potStackCollected(name: String, stack: Int): Unit =
    bettingStackManagement.report(name, stack)

}
