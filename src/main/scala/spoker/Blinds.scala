package spoker

import spoker.stack.StackHolder

package object blinds {

  case class Blinds(smallBlind: Int, bigBlind: Int)

  trait BlindsGathering[A] extends StackHolder[A] {

    val blinds: Blinds

    def collectSmallBlindFrom[B <: StackHolder[B]] = collectBlindFrom[B](blinds.smallBlind) _

    def collectBigBlindFrom[B <: StackHolder[B]] = collectBlindFrom[B](blinds.bigBlind) _

    protected def collectBlindFrom[B <: StackHolder[B]](stack: Int)(from: B): (A, B) =
      (this.collect(stack), from.submit(stack))
  }

}