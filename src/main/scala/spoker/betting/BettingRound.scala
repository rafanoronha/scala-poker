package spoker.betting

import scala.util.{ Failure, Success, Try }
import scala.collection.mutable.LinkedHashSet

import spoker.ManageablePlayer
import spoker.betting.stack.Pot 

object BettingRound {
  def preFlop(
    players: Seq[ManageablePlayer],
    bettersToAct: Seq[ManageablePlayer],
    smallBlind: ManageablePlayer,
    bigBlind: ManageablePlayer,
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
    players: Seq[ManageablePlayer],
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
  players: Seq[ManageablePlayer],
  currentBet: Bet,
  pot: Pot) extends AnyRef with BettingRoundExtractors with PlayersPositioning {

  val inTurn: Option[ManageablePlayer] = Try(currentBet.bettersToAct.next).map(Some(_)).getOrElse(None)

  val hasEnded = (1 == players.filter(_.isActive).size) || !betIsOpen

  def betIsOpen = currentBet.value > 0 && currentBet.matchedBy.toSet !=
    (players.filter(_.isActive).diff(currentBet.placedBy.manageablePlayer :: Nil)).toSet

  def place(ba: BetterAction): Bet = {
    val (action, better, placedBy) = (ba.action, ba.better, currentBet.placedBy)
    Try((action, better, placedBy, kind) match {
      case (_, OtherThanInTurn(), _, _) => throw new OutOfTurnException
      case (Check, _, BigBlind(), PreFlop) if currentBet.value == pot.blinds.bigBlind => None
      case (Check, _, _, _) if currentBet.value > 0 => throw new CantCheckException
      case (Check, _, _, _) => None
      case (Raise(value), placedBy, _, _) => {
        pot.collectUntil(value)(from = placedBy)
        Some(Bet(
          value = value,
          placedBy = placedBy,
          bettersToAct = newBetContenders(placedBy)))
      }
      case (Fold, player, _, _) => None
      case (Call, player, _, _) => {
        pot.collectUntil(currentBet.value)(from = player)
        Some(currentBet.copy(
          matchedBy = player.manageablePlayer +: currentBet.matchedBy))
      }
    }) match {
      case Success(updatedBet) => updatedBet.getOrElse(currentBet)
      case Failure(e) => throw e
    }
  }

  private def newBetContenders(better: ManageablePlayer): Iterator[ManageablePlayer] =
    LinkedHashSet((currentBet.bettersToAct.toList ++
        players.filter(_.isActive).diff(better :: Nil)): _*).iterator
}
