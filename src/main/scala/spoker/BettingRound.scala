package spoker

import scala.collection.mutable.LinkedHashSet
import scala.util.{Failure, Success, Try}
import spoker.table._
import spoker.stack.{MoveStack, StackHolder}
import spoker.blinds.{Blinds, BlindsGathering}

package object betting {

  object BettingRound {
    def apply(
               bs: Seq[Better],
               kind: RoundKind.Value = RoundKind.PreFlop,
               blinds: Blinds = Blinds(smallBlind = 1, bigBlind = 2)) = {
      val (p, bigBlind) = Pot(contenders = bs, blinds = blinds).collectBigBlindFrom(bs.find {
        BigBlind == _.position
      }.get)
      val (pot, smallBlind) = p.collectSmallBlindFrom(bs.find {
        SmallBlind == _.position
      }.get)
      val betters = bs.updated(bs.indexOf(bigBlind), bigBlind).updated(bs.indexOf(smallBlind), smallBlind)
      val bettersToAct = betters.iterator
      new BettingRound(
        kind,
        betters,
        pot,
        Some(bettersToAct.next()),
        Bet(pot = pot, value = blinds.bigBlind, placedBy = bigBlind, bettersToAct = bettersToAct))
    }
  }

  class BettingRound private(
                              val kind: RoundKind.Value,
                              val betters: Seq[Better],
                              val pot: Pot,
                              private val inTurn: Option[Better],
                              private val currentBet: Bet
                              ) {
    val hasEnded = (1 == pot.contenders.size) || !(currentBet isOpen)

    val nextInTurn = Try(currentBet.bettersToAct.next) match {
      case Success(p) => Some(p)
      case Failure(_) => None
    }

    val contenders = pot.contenders

    // TODO: gather 2nd half of sb for all sb's actions but fold
    def place(ba: BetterAction) = Try((ba.action, ba.better, currentBet.placedBy) match {
      case (_, OtherThanInTurn(), _) => throw new OutOfTurnException
      case (Check, NonBigBlind(), _) => throw new NonBigBlindCheckException
      case (Check, _, OtherThanInTurn()) => throw new CantCheckException
      case (Check, _, BigBlind()) => (None, None, None)
      case (Raise(value), placedBy, _) => {
        val (updatedBetter, updatedPot) = MoveStack(value - currentBet.value, from = placedBy, to = pot)
        (Some(betters.updated(betters.indexOf(placedBy), updatedBetter)),
          Some(updatedPot),
          Some(Bet(
            pot = updatedPot,
            value = value,
            placedBy = updatedBetter,
            bettersToAct = newBetContenders(updatedBetter))))
      }
      case (Fold, player, _) => {
        val updatedPot = pot gaveUp player
        (None,
          Some(updatedPot),
          Some(currentBet.copy(updatedPot)))
      }
      case (Call, player, _) => {
        val (updatedBetter, updatedPot) = MoveStack(currentBet.value, from = player, to = pot)
        (Some(betters.updated(betters.indexOf(player), updatedBetter)),
          Some(updatedPot),
          Some(currentBet.copy(
            pot = updatedPot,
            matchedBy = updatedBetter +: currentBet.matchedBy)))
      }
    }) match {
      case Success((updatedBetters, updatedPot, updatedBet)) =>
        new BettingRound(
          kind,
          updatedBetters.getOrElse(betters),
          updatedPot.getOrElse(pot),
          nextInTurn,
          updatedBet.getOrElse(currentBet))
      case Failure(e) => throw e
    }

    private def newBetContenders(better: Better): Iterator[Better] =
      LinkedHashSet((currentBet.bettersToAct.toList ++ contenders.diff(better :: Nil)): _*).iterator

    object OtherThanInTurn {
      def unapply(b: Better) = b != inTurn.getOrElse(null)
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

  }

  case class Bet(
                  pot: Pot,
                  value: Int,
                  placedBy: Better,
                  bettersToAct: Iterator[Better],
                  matchedBy: Seq[Better] = Nil
                  ) {
    def isOpen = matchedBy.toSet !=
      (pot.contenders.diff(placedBy :: Nil)).toSet
  }

  case class Better(positionedPlayer: PositionedPlayer) extends StackHolder[Better] {

    val stack = player.stack

    lazy val myAction = BetterAction(this)

    def myActionIs(a: Action) = myAction(a)

    def call = myActionIs(Call)

    def check = myActionIs(Check)

    def raise(value: Int) = myActionIs(Raise(value))

    def fold = myActionIs(Fold)

    def collect(stack: Int) = copy(positionedPlayer.collect(stack))

    def submit(stack: Int) = copy(positionedPlayer.submit(stack))

    def player: Player = positionedPlayer.player

    def position = positionedPlayer.position

    override def equals(that: Any) = that match {
      case t: Better => this.player == t.player
      case p: Player => this.player == p
      case _ => false
    }

    override def hashCode = player.##
  }

  case class Pot(contenders: Seq[Better], stack: Int = 0, blinds: Blinds) extends BlindsGathering[Pot] {
    def collect(stack: Int) = copy(stack = this.stack + stack)

    def submit(stack: Int) = copy(stack = this.stack - stack)

    def gaveUp(better: Better) = copy(contenders = contenders.diff(better :: Nil))
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

  implicit def betterFromPositionedPlayer(positionedPlayer: PositionedPlayer): Better =
    Better(positionedPlayer)

  implicit def bettersFromPositionedPlayers(positionedPlayers: Seq[PositionedPlayer]): Seq[Better] =
    positionedPlayers map betterFromPositionedPlayer

  implicit def playersFromBetters(betters: Seq[Better]): Seq[Player] = betters map {
    _.player
  }

  implicit def playerFromBetter(better: Better): Player = better.player

  implicit def fromTuple(tuple: BetterActionTuple): BetterAction = BetterAction(tuple._1, tuple._2)

}
