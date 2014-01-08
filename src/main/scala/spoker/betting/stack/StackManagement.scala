package spoker.betting.stack

import scala.collection.mutable.Map
import spoker.internals.MutableStateManagement

trait StackManagement extends MutableStateManagement[Int, Int] {
  override def updateState(holderName: String, su: Int): Int =
    currentState(holderName) + su
}