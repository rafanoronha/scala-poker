package spoker.betting.stack

import scala.collection.mutable.Map

object StackManagement {
  private val status: GlobalStatus = Map()

  def startManaging(tableName: String, tableStatus: TableStatus): Unit = status += tableName -> tableStatus

  def stopManaging(tableName: String): Unit = status -= tableName

  def report(data: UpdatedStackReport): Unit = {
    val (table: TableStatus, holderName: String) = (status(data._1), data._2)
    val stack = table(holderName)
    table.update(holderName, stack + data._3)
  }

  def holderStack(tableName: String, holderName: String): Int = status(tableName)(holderName)
}