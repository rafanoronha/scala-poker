package spoker

import scala.collection.mutable.LinkedHashSet
import scala.util.{Failure, Success, Try}
import spoker.table._

package object betting {

  object BettingRound {
    def apply(betters: Seq[Better], kind: RoundKind.Value = RoundKind.PreFlop) = {
      val bigBlind = betters.find {
        BigBlind == _.position
      }.get
      val bettersToAct = betters.iterator
      val pot = new Pot(betters)
      new BettingRound(
        kind,
        betters,
        pot,
        Some(bettersToAct.next()),
        bettersToAct,
        new Bet(pot, 2, bigBlind))
    }
  }

  class BettingRound private(
                              val kind: RoundKind.Value,
                              val betters: Seq[Better],
                              private val pot: Pot,
                              private val inTurn: Option[Better],
                              private val bettersToAct: Iterator[Better],
                              private val currentBet: Bet
                              ) {
    val hasEnded = (1 == pot.contenders.size) || !(currentBet isOpen)

    val nextInTurn = Try(bettersToAct next) match {
      case Success(p) => Some(p)
      case Failure(_) => None
    }

    val contenders = pot.contenders

    def place(action: Action) = Try((action, action.better, currentBet.placedBy) match {
      case (_, OtherThanInTurn(), _) => throw new OutOfTurnException
      case (_: Check, NonBigBlind(), _) => throw new NonBigBlindCheckException
      case (_: Check, _, OtherThanInTurn()) => throw new CantCheckException
      case (_: Check, _, BigBlind()) => (None, None, None)
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
      case Success((updatedPot, updatedBet, updatedBettersToAct)) =>
        new BettingRound(
          kind,
          betters,
          updatedPot.getOrElse(pot),
          nextInTurn,
          updatedBettersToAct.getOrElse(bettersToAct),
          updatedBet.getOrElse(currentBet))
      case Failure(e) => throw e
    }

    private def newBetContenders(better: Better): Iterator[Better] = {
      val contenders: Seq[Better] = pot.contenders map {
        (p) => (betters find {
          p == _.player
        }).get
      }
      LinkedHashSet((bettersToAct.toList ++ contenders.diff(better :: Nil)): _*).iterator
    }

    object OtherThanInTurn {
      def unapply(b: Better) = b != inTurn.getOrElse(null)
    }

    object BigBlind {
      def unapply(b: Better) = Position.BigBlind == b.position
    }

    object NonBigBlind {
      def unapply(b: Better) = Position.BigBlind != b.position
    }

  }

  class Bet(val pot: Pot, val value: Int, val placedBy: Better, val matchedBy: Seq[Better] = Nil) {
    def isOpen = (playersFromBetters(matchedBy)).toSet !=
      (pot.contenders.diff(placedBy.player :: Nil)).toSet
  }

  class Better(val positionedPlayer: PositionedPlayer) {
    def call = new Call(this)

    def check = new Check(this)

    def raise(value: Int) = new Raise(value, this)

    def fold = new Fold(this)

    def player = positionedPlayer.player

    def position = positionedPlayer.position

    override def equals(that: Any): Boolean = that match {
      case b: Better => player equals (b player)
      case _ => false
    }

    override def hashCode(): Int = player.##
  }

  sealed abstract class Action(val better: Better)

  class Call(better: Better) extends Action(better)

  class Check(better: Better) extends Action(better)

  class Raise(val value: Int, better: Better) extends Action(better)

  class Fold(better: Better) extends Action(better)

  object RoundKind extends Enumeration {
    val PreFlop, Flop, Turn, River = Value
  }

  class OutOfTurnException extends Exception

  class NonBigBlindCheckException extends Exception

  class CantCheckException extends Exception

  implicit def betterFromPositionedPlayer(positionedPlayer: PositionedPlayer): Better =
    new Better(positionedPlayer)

  implicit def bettersFromPositionedPlayers(positionedPlayers: Seq[PositionedPlayer]): Seq[Better] =
    positionedPlayers map betterFromPositionedPlayer

  implicit def playersFromBetters(betters: Seq[Better]): Seq[Player] = betters map {
    _.player
  }

  implicit def playerFromBetter(better: Better): Player = better.player

}
