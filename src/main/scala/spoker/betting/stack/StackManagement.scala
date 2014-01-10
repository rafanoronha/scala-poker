package spoker.betting.stack

import scala.collection.mutable.Map
import spoker.internals.MutableStateManagement

trait StackManagement extends MutableStateManagement[Double, Double] {
  override def updateState(holderName: String, su: Double): Double =
    currentState(holderName) + su
}