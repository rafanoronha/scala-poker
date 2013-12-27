package spoker.betting.stack

trait BlindsGathering extends StackHolder {

  val blinds: Blinds

  def collectSmallBlindFrom: StackHolder => Unit =
    collectBlind(blinds.smallBlind) _

  def collectBigBlindFrom: StackHolder => Unit =
    collectBlind(blinds.bigBlind) _

  private def collectBlind(stack: Int)(from: StackHolder): Unit =
    (this.collect(stack), from.submit(stack))
}
