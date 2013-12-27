package spoker.betting.stack

trait StackHolder {

  val name: String

  val stackManagement: StackManagement

  def stack: Int = stackManagement.currentState(name)

  def collect(stack: Int)(from: StackHolder): Unit =
    (this.collected(stack), from.submited(stack))

  def collected(stack: Int): Unit = reportToManagement(+stack)

  def submited(stack: Int): Unit = reportToManagement(-stack)

  private def reportToManagement(stackUpdate: Int): Unit =
    stackManagement.report(name, stackUpdate)

}
