package spoker

package object betting {

  object BettingRound {
    def apply(players: Seq[Player]) = {
      new BettingRound(players.iterator)
    }
  }

  class BettingRound private(players: Iterator[Player], val bettingActionGotPlaced: Boolean = false) {
    val inTurn = players.next()

    def place(action: Action) = {
      if (inTurn != action.player) {
        throw new OutOfTurnException
      }
      if (action.isInstanceOf[Check]) {
        if (action.player.isInstanceOf[BigBlindPlayer]) {
          if (bettingActionGotPlaced) {
            throw new CantCheckException
          }
        } else {
          throw new NonBigBlindCheckException
        }
      }
      if (players.hasNext) {
        new BettingRound(players, bettingActionGotPlaced || action.isInstanceOf[Raise])
      } else {
        this
      }
    }
  }

  sealed abstract class Action(val player: Player)

  class Call(player: Player) extends Action(player)

  class Check(player: Player) extends Action(player)

  class Raise(value: Int, player: Player) extends Action(player)

  class Player {
    def call = new Call(this)

    def check = new Check(this)

    def raise(value: Int) = new Raise(value, this)
  }

  class BigBlindPlayer extends Player

  class OutOfTurnException extends Exception

  class NonBigBlindCheckException extends Exception

  class CantCheckException extends Exception

}