package spoker.dealer

import spoker.internals.MutableStateManagement
import spoker.{ Card, Cards }

class CardsManagement extends MutableStateManagement[Cards, Card] {
  override def updateState(holderName: String, su: Card): Cards =
    su +: currentState(holderName)

  override def initialState(holderName: String): Cards = Nil
}