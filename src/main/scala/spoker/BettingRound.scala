package spoker

import scala.collection.mutable.LinkedHashSet
import scala.util.{Failure, Success, Try}
import spoker.table._
import spoker.stack.MoveStack

package object betting {

  val PreFlop = RoundKind.PreFlop
  
  object BettingRound {
    def preFlop(
                 bs: => Seq[Better],
                 pot: => Pot) = {
      val bb = bs.find {
        BigBlind == _.position
      }.get
      pot.collectBigBlindFrom(bb)
      pot.collectSmallBlindFrom(bs.find {
        SmallBlind == _.position
      }.get)
      new BettingRound(
        kind = PreFlop,
        bs = bs,
        currentBet = Bet(value = pot.blinds.bigBlind, placedBy = bb, bettersToAct = bs.iterator),
        pot = pot
      )
    }

    def nextRound(
                   kind: RoundKind.Value,
                   bs: => Seq[Better],
                   pot: => Pot,
                   currentBet: Bet) = {
      new BettingRound(
        kind = kind,
        bs = bs,
        currentBet = currentBet,
        pot = pot
      )
    }
  }

  class BettingRound private(
                              val kind: RoundKind.Value,
                              bs: => Seq[Better],
                              val currentBet: Bet,
                              pot: => Pot) {

    def copy(
              kind: RoundKind.Value = this.kind,
              bs: => Seq[Better] = this.betters,
              currentBet: Bet = this.currentBet,
              pot: => Pot = this.pot) =
      new BettingRound(
        kind = kind,
        bs = bs,
        pot = pot,
        currentBet = currentBet
      )

    val inTurn: Option[Better] = Try(currentBet.bettersToAct.next).map(Some(_)).getOrElse(None)

    val hasEnded = (1 == betters.size) || !betIsOpen

    def betters = bs

    def betIsOpen = currentBet.matchedBy.toSet !=
      (betters.diff(currentBet.placedBy :: Nil)).toSet

    def place(ba: BetterAction): Bet = {
      val (action, better, placedBy) = (ba.action, ba.better, currentBet.placedBy)
      (kind, action, better) match {
        case (PreFlop, OtherThanFold(), SmallBlind()) => {
          pot.collectSmallBlindFrom(better)
        }
        case _ => ()
      }        
      Try((action, better, placedBy) match {
	      case (_, OtherThanInTurn(), _) => throw new OutOfTurnException
	      case (Check, NonBigBlind(), _) => throw new NonBigBlindCheckException
	      case (Check, _, OtherThanInTurn()) => throw new CantCheckException
	      case (Check, _, BigBlind()) => None
	      case (Raise(value), placedBy, _) => {
	        MoveStack(value, from = placedBy, to = pot)
	        Some(Bet(
	          value = value,
	          placedBy = placedBy,
	          bettersToAct = newBetContenders(placedBy)))
	      }
	      case (Fold, player, _) => None
	      case (Call, player, _) => {
	        MoveStack(currentBet.value, from = player, to = pot)
	        Some(currentBet.copy(
	          matchedBy = player +: currentBet.matchedBy))
	      }
	    }) match {
	      case Success(updatedBet) => updatedBet.getOrElse(currentBet)
	      case Failure(e) => throw e
      }
    }

    private def newBetContenders(better: Better): Iterator[Better] =
      LinkedHashSet((currentBet.bettersToAct.toList ++ betters.diff(better :: Nil)): _*).iterator

    object OtherThanInTurn {
      def unapply(b: Better) = inTurn match {
        case Some(x) => b != x
        case None => true
      }
    }

    object BigBlind {
      def unapply(b: Better) = Position.BigBlind == b.position
    }

    object SmallBlind {
      def unapply(b: Better) = Position.SmallBlind == b.position
    }

    object NonBigBlind {
      def unapply(b: Better) = Position.BigBlind != b.position
    }

    object OtherThanFold {
      def unapply(a: Action) = Fold != a
    }        
  }

  case class Bet(
                  value: Int,
                  placedBy: Better,
                  bettersToAct: Iterator[Better],
                  matchedBy: Seq[Better] = Nil
                  )

  case class Better(positionedPlayer: PositionedPlayer) {

    lazy val myAction = BetterAction(this)

    def myActionIs(a: Action) = myAction(a)

    def call = myActionIs(Call)

    def check = myActionIs(Check)

    def raise(value: Int) = myActionIs(Raise(value))

    def fold = myActionIs(Fold)

    override def equals(that: Any) = that match {
      case b: Better => playerFromBetter(this) == playerFromBetter(b)
      case pp: PositionedPlayer => playerFromBetter(this) == pp.player
      case p: Player => playerFromBetter(this) == p
    }

    override def hashCode = playerFromBetter(this).##
  }

  sealed trait Action

  object Call extends Action

  object Check extends Action

  object Fold extends Action

  case class Raise(value: Int) extends Action

  object BetterAction {
    def apply(better: Better) = {
      (a: Action) => new BetterAction(a, better)
    }
  }

  case class BetterAction(action: Action, better: Better)

  type BetterActionTuple = (Action, Better)

  object RoundKind extends Enumeration {
    val PreFlop, Flop, Turn, River = Value
  }

  class OutOfTurnException extends Exception

  class NonBigBlindCheckException extends Exception

  class CantCheckException extends Exception

  implicit def positionedPlayerFroBetter(better: Better): PositionedPlayer =
    better.positionedPlayer

  implicit def betterFromPositionedPlayer(positionedPlayer: PositionedPlayer): Better =
    Better(positionedPlayer)

  implicit def bettersFromPositionedPlayers(positionedPlayers: Seq[PositionedPlayer]): Seq[Better] =
    positionedPlayers map betterFromPositionedPlayer

  implicit def playersFromBetters(betters: Seq[Better]): Seq[Player] = betters map {
    playerFromBetter(_)
  }

  implicit def playerFromBetter(better: Better): Player = better.positionedPlayer.player

  implicit def fromTuple(tuple: BetterActionTuple): BetterAction = BetterAction(tuple._1, tuple._2)

}
