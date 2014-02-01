package spoker.betting

import scala.util.{ Failure, Success, Try }
import scala.collection.mutable.LinkedHashSet
import spoker.ManageablePlayer

object BettingRound {
  def preFlop(
    players: Seq[ManageablePlayer],
    bettersToAct: Seq[ManageablePlayer],
    smallBlind: ManageablePlayer,
    bigBlind: ManageablePlayer,
    stackManager: StackManager) = {
    
    val bettingState = BettingState(bettersToAct = bettersToAct)
    
    stackManager.collectSmallBlind(smallBlind)
    stackManager.collectBigBlind(bigBlind)

    new BettingRound(
      kind = PreFlop,
      players = players,
      bettingState = bettingState,
      stackManager = stackManager)
  }

  def nextRound(
    kind: RoundKind.Value,
    players: Seq[ManageablePlayer],
    stackManager: StackManager,
    bettingState: BettingState) = {
    new BettingRound(
      kind = kind,
      players = players,
      bettingState = bettingState,
      stackManager = stackManager)
  }
  
}

case class BettingRound private (
  kind: RoundKind.Value,
  players: Seq[ManageablePlayer],
  bettingState: BettingState,
  stackManager: StackManager) extends AnyRef with PlayersPositioning {
  
  val inTurn: Option[ManageablePlayer] = bettingState.bettersToAct.headOption

  val hasEnded = bettingState.bettersToAct.isEmpty

  def betIsOpen = stackManager.currentBet.isDefined

  def place(ba: BetterAction): BettingState = {
    val better = ba.better
    
    if (hasEnded || better != inTurn.get)
      throw new OutOfTurnException
      
    def placeFold = bettingState.copy(bettersToAct = bettingState.bettersToAct.tail)
    
    def placeCheck = {
      stackManager.check(better)
      bettingState.copy(
        bettersToAct = bettingState.bettersToAct.tail,
        bettersActed = bettingState.bettersActed :+ bettingState.bettersToAct.head)
    }
    
    def placeCall = {
      stackManager.call(better)
      bettingState.copy(
        bettersToAct = bettingState.bettersToAct.tail,
        bettersActed = bettingState.bettersActed :+ bettingState.bettersToAct.head)
    }
    
    def placeBet(bet: Bet) = {
      stackManager.bet(better, bet.value)
      bettingState.copy(
        bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
        bettersActed = better.manageablePlayer :: Nil)
    }

    def placeRaise(raise: Raise) = {
      stackManager.raise(better, raise.value)
      bettingState.copy(
        bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
        bettersActed = bettingState.bettersToAct.head :: Nil)
    }
    
    def placeAllIn = {
      stackManager.allIn(better)
      if (better.positionedPlayer == stackManager.betPlacedBy.get) {
        bettingState.copy(
          bettersToAct = bettingState.bettersToAct.tail ++ bettingState.bettersActed,
          bettersActed = Nil)
      } else {
        bettingState.copy(bettersToAct = bettingState.bettersToAct.tail)
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
