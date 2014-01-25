package spoker.betting.stack

case class Pot(
  blinds: Blinds,
  stackManagement: StackManagement) extends AnyRef with BlindsGathering {

  val name = "Pot"

  val bettingStackManagement = new StackManagement {
    def initialState(holderName: String): Double = 0
  }

  def collectUntil(matching: Double)(from: StackHolder): Unit = {
    val value = matching - potStackByPlayer(from.name)
    collect(value)(from)
  }

  def collectUntilMatching(amountBettedBy: StackHolder)(from: StackHolder): Unit = {
    val amount = potStackByPlayer(amountBettedBy.name) - potStackByPlayer(from.name)
    collect(amount)(from)
  }

  override def collect(stack: Double)(from: StackHolder): Unit = {
    super.collect(stack)(from)
    potStackCollected(from.name, stack)
  }

  def potStackByPlayer(name: String): Double =
    bettingStackManagement.currentState(name)

  private def potStackCollected(name: String, stack: Double): Unit =
    bettingStackManagement.report(name, stack)

}
