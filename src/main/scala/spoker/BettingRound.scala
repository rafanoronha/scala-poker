package spoker

import scala.collection.mutable.LinkedHashSet
import scala.util.{Failure, Success, Try}

package object betting {

  object BettingRound {
    def apply(players: Seq[Player]) = {
      val bigBlind = players.find {
        _.isInstanceOf[BigBlindPlayer]
      }.get
      val pot = new Pot(players)
      val playersIterator = players.iterator
      new BettingRound(
        playersIterator,
        pot,
        Some(playersIterator.next()),
        new Bet(pot, 2, bigBlind))
    }
  }

  class BettingRound private(
                              players: Iterator[Player],
                              val pot: Pot,
                              val inTurn: Option[Player],
                              private val currentBet: Bet
                              ) {
    val hasEnded = (1 == pot.contenders.size) || !(currentBet isOpen)
    val nextInTurn = Try(players next) match {
      case Success(p) => Some(p)
      case Failure(_) => None
    }

    def place(action: Action) = Try((action, action.player, currentBet.placedBy) match {
      case (_, OtherThanInTurn(), _) => throw new OutOfTurnException
      case (_: Check, NonBigBlind(), _) => throw new NonBigBlindCheckException
      case (_: Check, _, OtherThanInTurn()) => throw new CantCheckException
      case (_: Check, _, _: BigBlindPlayer) => (None, None, None)
      case (raise: Raise, placedBy, _) => (
        None,
        Some(new Bet(pot, raise.value, placedBy)),
        Some(newBetContenders(placedBy)))
      case (_: Fold, player, _) => {
        val updatedPot = pot gaveUp player
        (Some(updatedPot), Some(new Bet(updatedPot, currentBet.value, currentBet.placedBy)), None)
      }
      case (_: Call, player, _) =>
        (None, Some(new Bet(pot, currentBet.value, currentBet.placedBy, player +: currentBet.matchedBy)), None)
    }) match {
      case Success((updatedPot, updatedBet, updatedPlayers)) =>
        new BettingRound(
          updatedPlayers.getOrElse(players),
          updatedPot.getOrElse(pot),
          nextInTurn,
          updatedBet.getOrElse(currentBet))
      case Failure(e) => throw e
    }

    private def newBetContenders = (better: Player) =>
      LinkedHashSet((players.toList ++ pot.contenders.diff(better :: Nil)): _*).iterator

    private object OtherThanInTurn {
      def unapply(p: Player) = p != inTurn.get
    }

    private object NonBigBlind {
      def unapply(p: Player) = !p.isInstanceOf[BigBlindPlayer]
    }

  }

  class Pot(val contenders: Seq[Player]) {
    def gaveUp = (player: Player) => new Pot(contenders.diff(player :: Nil))
  }

  class Bet(val pot: Pot, val value: Int, val placedBy: Player, val matchedBy: Seq[Player] = Nil) {
    def isOpen = !(matchedBy containsSlice (pot.contenders.diff(placedBy :: Nil)))
  }

  sealed abstract class Action(val player: Player)

  class Call(player: Player) extends Action(player)

  class Check(player: Player) extends Action(player)

  class Raise(val value: Int, player: Player) extends Action(player)

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
