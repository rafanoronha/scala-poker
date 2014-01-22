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
        bettersToAct = bettersToAct),
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

  val inTurn: Option[ManageablePlayer] = bettingState.bettersToAct.headOption

  val hasEnded = bettingState.bettersToAct.isEmpty

  def betIsOpen = bettingState.currentBet.isDefined

  def place(ba: BetterAction): BettingState = {
    val (action, better) = (ba.action, ba.better)
    Try((action, better, kind) match {
      case (_, OtherThanInTurn(), _) => throw new OutOfTurnException
      case (Check, BigBlind(), PreFlop) if (bettingState.currentBet.get.value == pot.blinds.bigBlind) => Some(
          bettingState.copy(
              bettersToAct = bettingState.bettersToAct.tail,
              bettersActed = bettingState.bettersActed :+ bettingState.bettersToAct.head))
      case (Check, _, _) if bettingState.currentBet.isDefined => throw new CantCheckException
      case (Check, _, _) => Some(
          bettingState.copy(
              bettersToAct = bettingState.bettersToAct.tail,
              bettersActed = bettingState.bettersActed :+ bettingState.bettersToAct.head))
      case (bet @ Bet(value), better, _) => {
        if (betIsOpen) throw new CantBetException
        pot.collect(value)(from = better)
        Some(BettingState(
          currentBet = Some(bet),
          placedBy = better,
          bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
          bettersActed = better.manageablePlayer :: Nil))
      }
      case (raise @ Raise(value), better, _) => {
        if (!betIsOpen) throw new CantRaiseException
        pot.collectUntil(value)(from = better)
        Some(BettingState(
          currentBet = Some(raise),
          placedBy = better,
          bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
          bettersActed = bettingState.bettersToAct.head :: Nil))
      }
      case (Fold, player, _) => Some(
          bettingState.copy(bettersToAct = bettingState.bettersToAct.tail))

      case (Call, player, _) => {
        pot.collectUntilMatching(amountBettedBy = bettingState.placedBy)(from = player)
        Some(bettingState.copy(
          bettersToAct = bettingState.bettersToAct.tail,
          bettersActed = bettingState.bettersActed :+ bettingState.bettersToAct.head))
      }
      case (AllIn, player, _) => {
        val amount = player.stack
        pot.collect(amount)(from = player)
        bettingState.currentBet match {
          case Some(Raise(earlierRaiseAmount)) if (amount <= earlierRaiseAmount) =>
            Some(bettingState.copy(bettersToAct = bettingState.bettersToAct.tail))
          case _ => 
            Some(bettingState.copy(
              currentBet = Some(Raise(amount)),
              placedBy = player,
              bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
              bettersActed = Nil))
        }
      }
    }) match {
      case Success(updatedBet) => updatedBet.getOrElse(bettingState)
      case Failure(e) => throw e
    }
  }
}
