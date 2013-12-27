package spoker.internals

import scala.collection.mutable.Map
import scala.util.Try

trait MutableStateManagement[State, StateUpdate] {

  private val state: Map[String, State] = Map()

  def updateState(holderName: String, su: StateUpdate): State

  def initialState(holderName: String): State

  def report(holderName: String, su: StateUpdate): Unit = {
    state.update(holderName, updateState(holderName, su))
  }

  def currentState(holderName: String): State = Try(state(holderName)).getOrElse(initialState(holderName))
}
