package spoker

package object stack {

  trait StackHolder[T] {

    val stack: Int

    def collect(stack: Int): T

    def submit(stack: Int): T

  }

  object MoveStack {
    def apply[A <: StackHolder[A], B <: StackHolder[B]](stack: Int, from: A, to: B): (A, B) =
      (from.submit(stack), to.collect(stack))
  }

}