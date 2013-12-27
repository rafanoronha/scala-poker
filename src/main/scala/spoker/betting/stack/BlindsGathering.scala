package spoker.betting.stack

trait BlindsGathering extends StackHolder {

  val blinds: Blinds
  
  def collectSmallBlindFrom: StackHolder => Unit =
    collect(blinds.smallBlind) _

  def collectBigBlindFrom: StackHolder => Unit =
    collect(blinds.bigBlind) _
}
