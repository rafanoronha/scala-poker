package spoker.betting.stack

trait StackHolder {

  val tableName: String

  val name: String

  def stack: Int = StackManagement.holderStack(tableName, name)

  def collect(stack: Int): Unit = reportToManagement(+stack)

  def submit(stack: Int): Unit = reportToManagement(-stack)

  private def reportToManagement(stackUpdate: Int): Unit =
    StackManagement.report(tableName, name, stackUpdate)

}
