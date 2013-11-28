package spoker.betting.stack

trait StackHolder {

  val stack: Int

  val chief: StackHolderChief

  def collect(stack: Int): Unit = reportToChief(+stack)

  def submit(stack: Int): Unit = reportToChief(-stack)

  private def reportToChief(x: Int): Unit = chief.report((this, x))

}
