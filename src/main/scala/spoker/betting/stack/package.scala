package spoker.betting

import scala.collection.mutable.Map

package object stack {
  type TableStatus = Map[String, Int] 
  
  type GlobalStatus = Map[String, TableStatus]
  
  type UpdatedStackReport = (String, String, Int)

  object MoveStack {
    def apply(stack: Int, from: StackHolder, to: StackHolder): Unit = (from.submit(stack), to.collect(stack))
  }

  case class Blinds(smallBlind: Int, bigBlind: Int)

}