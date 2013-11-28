package spoker.betting

package object stack {
  type UpdatedStackReport = (StackHolder, Int)

  object MoveStack {
    def apply(stack: Int, from: StackHolder, to: StackHolder): Unit = (from.submit(stack), to.collect(stack))
  }

  trait StackHolderChief {
    def report(data: UpdatedStackReport): Unit
  }

  case class Blinds(smallBlind: Int, bigBlind: Int)

}