package spoker.betting.stack

trait StackHolder {

  val name: String

  val stackManagement: StackManagement

  def stack: Double = stackManagement.currentState(name)

  def collect(stack: Double)(from: StackHolder): Unit =
    (this.collected(stack), from.submited(stack))

  def collected(stack: Double): Unit = reportToManagement(+stack)

  def submited(stack: Double): Unit = reportToManagement(-stack)

  private def reportToManagement(stackUpdate: Double): Unit =
    stackManagement.report(name, stackUpdate)

}
