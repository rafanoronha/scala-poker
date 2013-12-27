package spoker.betting.stack

trait StackHolder {

  val tableName: String

  val name: String

  def stack: Int = PlayerStackManagement.currentState(tableName, name)

  def collect(stack: Int)(from: StackHolder): Unit =
    (this.collected(stack), from.submited(stack))

  def collected(stack: Int): Unit = reportToManagement(+stack)

  def submited(stack: Int): Unit = reportToManagement(-stack)

  private def reportToManagement(stackUpdate: Int): Unit =
    PlayerStackManagement.report(tableName, name, stackUpdate)

}
