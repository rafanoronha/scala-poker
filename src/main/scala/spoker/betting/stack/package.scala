package spoker.betting

import scala.collection.mutable.Map

package object stack {
  object MoveStack {
    def apply(stack: Int, from: StackHolder, to: StackHolder): Unit = (from.submit(stack), to.collect(stack))
  }

  case class Blinds(smallBlind: Int, bigBlind: Int)

}