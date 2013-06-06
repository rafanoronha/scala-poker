package spoker

package object betting {

  object BettingRound {
    def apply(players: Seq[Player]) = {
      new BettingRound(players.iterator)
    }
  }

  class BettingRound private(players: Iterator[Player]) {
    val inTurn = players.next()

    def place(action: Action) = {
      if (inTurn != action.player) {
        throw new OutOfTurnException
      }
      if (players.hasNext) {
        new BettingRound(players)
      } else {
        this
      }
    }
  }

  sealed abstract class Action(val player: Player)

  class Call(player: Player) extends Action(player)

  class Player {
    def call = new Call(this)
  }

  class OutOfTurnException extends Exception

}