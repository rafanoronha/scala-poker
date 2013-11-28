package spoker.betting

import scala.util.{Failure, Success, Try}
import spoker.betting.{BettingRoundExtractors => Is}
import spoker.betting.stack.{Pot, MoveStack}
import scala.collection.mutable.LinkedHashSet

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
      pot = pot)
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
      pot = pot)
  }
}

class BettingRound private (
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
      currentBet = currentBet)

  val inTurn: Option[Better] = Try(currentBet.bettersToAct.next).map(Some(_)).getOrElse(None)

  val hasEnded = (1 == betters.size) || !betIsOpen

  def betters = bs

  def betIsOpen = currentBet.matchedBy.toSet !=
    (betters.diff(currentBet.placedBy :: Nil)).toSet

  def place(ba: BetterAction): Bet = {
    val (action, better, placedBy) = (ba.action, ba.better, currentBet.placedBy)
    (kind, action, better) match {
      case (PreFlop, Is.OtherThanFold(), Is.SmallBlind()) => {
        pot.collectSmallBlindFrom(better)
      }
      case _ => ()
    }
    Try((action, better, placedBy) match {
      case (_, OtherThanInTurn(), _) => throw new OutOfTurnException
      case (Check, Is.NonBigBlind(), _) => throw new NonBigBlindCheckException
      case (Check, _, OtherThanInTurn()) => throw new CantCheckException
      case (Check, _, Is.BigBlind()) => None
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

  private object OtherThanInTurn {
    def unapply(b: Better) = inTurn match {
      case Some(x) => b != x
      case None => true
    }
  }
 

  private def newBetContenders(better: Better): Iterator[Better] =
    LinkedHashSet((currentBet.bettersToAct.toList ++ betters.diff(better :: Nil)): _*).iterator
}