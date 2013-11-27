package spoker

package object stack {

  type UpdatedStackReport = (StackHolder, Int)

  trait StackHolderChief {
    def report(data: UpdatedStackReport): Unit
  }

  trait StackHolder {

    val stack: Int

    val chief: StackHolderChief

    def collect(stack: Int): Unit = reportToChief(+stack)

    def submit(stack: Int): Unit = reportToChief(-stack)

    private def reportToChief(x: Int): Unit = chief.report((this, x))

  }

  object MoveStack {
    def apply(stack: Int, from: StackHolder, to: StackHolder): Unit = (from.submit(stack), to.collect(stack))
  }

}