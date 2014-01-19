package spoker.betting

import scala.util.{ Failure, Success, Try }
import scala.collection.mutable.LinkedHashSet

import spoker.ManageablePlayer
import spoker.betting.stack.{ StackHolder, Pot }

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
      bettingState = BettingState(
        currentBet = Some(Bet(pot.blinds.bigBlind)),
        placedBy = bigBlind,
        bettersToAct = bettersToAct.iterator),
      pot = pot)
  }

  def nextRound(
    kind: RoundKind.Value,
    players: Seq[ManageablePlayer],
    pot: Pot,
    bettingState: BettingState) = {
    new BettingRound(
      kind = kind,
      players = players,
      bettingState = bettingState,
      pot = pot)
  }
}

case class BettingRound private (
  kind: RoundKind.Value,
  players: Seq[ManageablePlayer],
  bettingState: BettingState,
  pot: Pot) extends AnyRef with BettingRoundExtractors with PlayersPositioning {

  val inTurn: Option[ManageablePlayer] = Try(bettingState.bettersToAct.next).map(Some(_)).getOrElse(None)

  val hasEnded = (1 >= players.filter(player => player.isActive && !player.isAllIn).size) || !betIsOpen

  def betIsOpen = bettingState.currentBet.isDefined && bettingState.matchedBy.toSet !=
    (players.filter(_.isActive).diff(bettingState.placedBy.manageablePlayer :: Nil)).toSet

  def place(ba: BetterAction): BettingState = {
    val (action, better, placedBy) = (ba.action, ba.better, bettingState.placedBy)
    Try((action, better, placedBy, kind) match {
      case (_, OtherThanInTurn(), _, _) => throw new OutOfTurnException
      case (Check, _, BigBlind(), PreFlop) if (bettingState.currentBet.get.value == pot.blinds.bigBlind) => None
      case (Check, _, _, _) if bettingState.currentBet.isDefined => throw new CantCheckException
      case (Check, _, _, _) => None
      case (bet @ Bet(value), placedBy, _, _) => {
        if (betIsOpen) throw new CantBetException
        pot.collect(value)(from = placedBy)
        Some(BettingState(
          currentBet = Some(bet),
          placedBy = placedBy,
          bettersToAct = newBetContenders(placedBy)))
      }
      case (raise @ Raise(value), placedBy, _, _) => {
        if (!betIsOpen) throw new CantRaiseException
        pot.collectUntil(value)(from = placedBy)
        Some(BettingState(
          currentBet = Some(raise),
          placedBy = placedBy,
          bettersToAct = newBetContenders(placedBy)))
      }
      case (Fold, player, _, _) => None
      case (Call, player, _, _) => {
        pot.collectUntilMatching(amountBettedBy = bettingState.placedBy)(from = player)
        Some(bettingState.copy(
          matchedBy = player.manageablePlayer +: bettingState.matchedBy))
      }
      case (AllIn, player, previousBetter, _) => {
        val amount = player.stack
        pot.collect(amount)(from = player)
        bettingState.currentBet match {
          case Some(Raise(earlierRaiseAmount)) if (amount < earlierRaiseAmount) =>
            Some(bettingState)
          case _ => 
            Some(bettingState.copy(
              currentBet = Some(Raise(amount)),
              placedBy = player))
        }
      }
    }) match {
      case Success(updatedBet) => updatedBet.getOrElse(bettingState)
      case Failure(e) => throw e
    }
  }

  private def newBetContenders(better: ManageablePlayer): Iterator[ManageablePlayer] =
    LinkedHashSet((bettingState.bettersToAct.toList ++
      players.filter(_.isActive).diff(better :: Nil)): _*).iterator
}
