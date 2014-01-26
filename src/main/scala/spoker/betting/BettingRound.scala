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
  pot: Pot) extends AnyRef with PlayersPositioning {

  val inTurn: Option[ManageablePlayer] = bettingState.bettersToAct.headOption

  val hasEnded = bettingState.bettersToAct.isEmpty

  def betIsOpen = bettingState.currentBet.isDefined

  def place(ba: BetterAction): BettingState = {
    val better = ba.better
    
    if (hasEnded || better != inTurn.get)
      throw new OutOfTurnException
      
    def placeFold = bettingState.copy(bettersToAct = bettingState.bettersToAct.tail)
    
    def placeCheck = {
      if (betIsOpen && (better != bigBlind || bettingState.currentBet.get.value != pot.blinds.bigBlind))
        throw new CantCheckException
      bettingState.copy(
        bettersToAct = bettingState.bettersToAct.tail,
        bettersActed = bettingState.bettersActed :+ bettingState.bettersToAct.head)
    }
    
    def placeCall = {
      if (better.stack <= (pot.potStackByPlayer(bettingState.placedBy.manageablePlayer.name) - pot.potStackByPlayer(better.manageablePlayer.name)))  {
        placeAllIn
      } else {
        pot.collectUntilMatching(amountBettedBy = bettingState.placedBy)(from = better)
        bettingState.copy(
          bettersToAct = bettingState.bettersToAct.tail,
          bettersActed = bettingState.bettersActed :+ bettingState.bettersToAct.head)
      }
    }
    
    def placeBet(bet: Bet) = {
      if (betIsOpen) throw new CantBetException

      if (better.stack <= bet.value) {
        placeAllIn
      } else {
        pot.collect(bet.value)(from = better)
        BettingState(
          currentBet = Some(bet),
          placedBy = better,
          bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
          bettersActed = better.manageablePlayer :: Nil)
      }
    }

    def placeRaise(raise: Raise) = {
      if (!betIsOpen) throw new CantRaiseException
      //todo: use a current pot stack of this player #45
      if (better.stack <= (raise.value - pot.potStackByPlayer(better.manageablePlayer.name))) {
        placeAllIn
      } else {
        pot.collectUntil(raise.value)(from = better)
        BettingState(
          currentBet = Some(raise),
          placedBy = better,
          bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
          bettersActed = bettingState.bettersToAct.head :: Nil)
      }
    }
    
    def placeAllIn = {
      val amount = better.stack
      pot.collect(amount)(from = better)
      bettingState.currentBet match {
        case Some(Raise(earlierRaiseAmount)) if (amount <= earlierRaiseAmount) =>
          bettingState.copy(bettersToAct = bettingState.bettersToAct.tail)
        case _ => 
          bettingState.copy(
            currentBet = Some(Raise(amount)),
            placedBy = better,
            bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
            bettersActed = Nil)
      }
    }

    ba.action match {
      case Fold => placeFold
      case Check => placeCheck
      case Call => placeCall
      case bet: Bet => placeBet(bet)
      case raise: Raise => placeRaise(raise)
      case AllIn => placeAllIn
    }
  }
}
