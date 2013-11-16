package spoker

import spoker.stack.StackHolder

package object blinds {

  case class Blinds(smallBlind: Int, bigBlind: Int)

  trait BlindsGathering extends StackHolder {

    val blinds: Blinds

    def collectSmallBlindFrom = collectBlind(blinds.smallBlind) _

    def collectBigBlindFrom = collectBlind(blinds.bigBlind) _

    private def collectBlind(stack: Int)(from: StackHolder): Unit =
      (this.collect(stack), from.submit(stack))
  }

}