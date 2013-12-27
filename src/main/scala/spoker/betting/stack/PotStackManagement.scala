package spoker.betting.stack

import scala.collection.mutable.Map
import spoker.internals.MSM.MutableStateManagement

trait PotStackManagement extends MutableStateManagement[Int, Int] {

  def potStackByPlayer(tableName: String, name: String): Int =
    currentState(tableName, name)

  def potStackCollected(tableName: String, name: String, stack: Int): Unit =
    report(tableName, name, stack)

  override def updateState(tableName: String, holderName: String, su: Int): Int =
    currentState(tableName, holderName) + su

  override def initialState(holderName: String): Int = 0
}