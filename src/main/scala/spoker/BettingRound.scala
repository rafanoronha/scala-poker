package spoker

package object betting {

  object BettingRound {
    def apply(players: Seq[Player]) = {
      new BettingRound(players.iterator)
    }
  }

  class BettingRound private(players: Iterator[Player], val bettingActionGotPlaced: Boolean = false) {
    val inTurn = players.next()

    def place(action: Action) = (action, action.player, bettingActionGotPlaced, players.hasNext) match {
      case (_, OutOfTurn(), _, _) => throw new OutOfTurnException
      case (_: Check, NonBigBlind(), _, _) => throw new NonBigBlindCheckException
      case (_: Check, _, true, _) => throw new CantCheckException
      case (_, _, _, true) => new BettingRound(players, bettingActionGotPlaced || action.isInstanceOf[Raise])
      case _ => this
    }

    object OutOfTurn {
      def unapply(p: Player) = p != inTurn
    }

    object NonBigBlind {
      def unapply(p: Player) = !p.isInstanceOf[BigBlindPlayer]
    }

  }

  sealed abstract class Action(val player: Player)

  class Call(player: Player) extends Action(player)

  class Check(player: Player) extends Action(player)

  class Raise(value: Int, player: Player) extends Action(player)

  class Fold(player: Player) extends Action(player)

  class Player {
    def call = new Call(this)

    def check = new Check(this)

    def raise(value: Int) = new Raise(value, this)

    def fold = new Fold(this)
  }

  class BigBlindPlayer extends Player

  class OutOfTurnException extends Exception

  class NonBigBlindCheckException extends Exception

  class CantCheckException extends Exception

}