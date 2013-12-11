package spoker.internals

import scala.collection.mutable.Map

package object MSM {

  type TableState[State] = Map[String, State]
  type GlobalState[State] = Map[String, TableState[State]]
  type UpdatedStateReport[StateUpdate] = (String, String, StateUpdate)

  trait MutableStateManagement[State, StateUpdate] {
    protected val state: GlobalState[State] = Map()

    def updateState(tableName: String, holderName: String, su: StateUpdate): State

    def initialState(holderName: String): State

    def startManaging(tableName: String, holders: Seq[String]): Unit = {
      val tableState = Map[String, State]()
      holders.foreach(h => tableState += h -> initialState(h))
      state += tableName -> tableState
    }

    def stopManaging(tableName: String): Unit = state -= tableName

    def report(data: UpdatedStateReport[StateUpdate]): Unit = {
      val (table: TableState[State], holderName: String) = (state(data._1), data._2)
      table.update(holderName, updateState(data._1, holderName, data._3))
    }

    def currentState(tableName: String, holderName: String): State = state(tableName)(holderName)
  }
}