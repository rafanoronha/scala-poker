package spoker.betting.stack

import scala.collection.mutable.Map
import spoker.internals.MSM.MutableStateManagement

object StackManagement extends MutableStateManagement[Int, Int] {
  override def updateState(tableName: String, holderName: String, su: Int): Int =
    currentState(tableName, holderName) + su

  override def initialState(holderName: String): Int =
    if (holderName.startsWith("Table")) 0
    else 50
}