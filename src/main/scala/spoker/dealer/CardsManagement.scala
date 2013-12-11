package spoker.dealer

import spoker.internals.MSM.MutableStateManagement
import spoker.{ Card, Cards }

object CardsManagement extends MutableStateManagement[Cards, Card] {
  override def updateState(tableName: String, holderName: String, su: Card): Cards =
    su +: currentState(tableName, holderName)

  override def initialState(holderName: String): Cards = Nil
}