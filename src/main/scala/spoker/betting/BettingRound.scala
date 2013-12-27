package spoker.betting

import scala.util.{ Failure, Success, Try }
import spoker.betting.stack.{ Pot, MoveStack }
import scala.collection.mutable.LinkedHashSet

object BettingRound {
  def preFlop(
    players: Seq[PositionedPlayer],
    bettersToAct: Seq[Better],
    smallBlind: PositionedPlayer,
    bigBlind: PositionedPlayer,
    pot: Pot) = {
    pot.collectBigBlindFrom(bigBlind)
    pot.collectSmallBlindFrom(smallBlind)
    new BettingRound(
      kind = PreFlop,
      players = players,
      currentBet = Bet(
        value = pot.blinds.bigBlind,
        placedBy = bigBlind,
        bettersToAct = bettersToAct.iterator),
      pot = pot)
  }

  def nextRound(
    kind: RoundKind.Value,
    players: Seq[PositionedPlayer],
    pot: Pot,
    currentBet: Bet) = {
    new BettingRound(
      kind = kind,
      players = players,
      currentBet = currentBet,
      pot = pot)
  }
}

case class BettingRound private (
  kind: RoundKind.Value,
  players: Seq[PositionedPlayer],
  currentBet: Bet,
  pot: Pot) extends AnyRef with BettingRoundExtractors with PlayersPositioning {

  val inTurn: Option[Better] = Try(currentBet.bettersToAct.next).map(Some(_)).getOrElse(None)

  val hasEnded = (1 == players.filter(_.isActive).size) || !betIsOpen

  def betters: Seq[Better] = players

  def betIsOpen = currentBet.value > 0 && currentBet.matchedBy.toSet !=
    (players.filter(_.isActive).diff(currentBet.placedBy :: Nil)).toSet

  def place(ba: BetterAction): Bet = {
    val (action, better, placedBy) = (ba.action, ba.better, currentBet.placedBy)
    def smallBlindDiscount(value: Int): Int =
      if (kind == PreFlop && better == smallBlind && currentBet.value == pot.blinds.bigBlind)
        value - pot.blinds.smallBlind
      else value
    Try((action, better, placedBy, kind) match {
      case (_, OtherThanInTurn(), _, _) => throw new OutOfTurnException
      case (Check, _, BigBlind(), PreFlop) if currentBet.value == pot.blinds.bigBlind => None
      case (Check, _, _, _) if currentBet.value > 0 => throw new CantCheckException
      case (Check, _, _, _) => None
      case (Raise(v), placedBy, _, _) => {
        val value = smallBlindDiscount(v)
        MoveStack(value, from = placedBy, to = pot)
        Some(Bet(
          value = value,
          placedBy = placedBy,
          bettersToAct = newBetContenders(placedBy)))
      }
      case (Fold, player, _, _) => None
      case (Call, player, _, _) => {
        val value = smallBlindDiscount(currentBet.value)
        MoveStack(value, from = player, to = pot)
        Some(currentBet.copy(
          matchedBy = player +: currentBet.matchedBy))
      }
    }) match {
      case Success(updatedBet) => updatedBet.getOrElse(currentBet)
      case Failure(e) => throw e
    }
  }

  private def newBetContenders(better: Better): Iterator[Better] =
    LinkedHashSet((currentBet.bettersToAct.toList ++ betters.filter(_.isActive).diff(better :: Nil)): _*).iterator
}
