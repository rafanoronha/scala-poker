package spoker

import scala.collection.mutable.Map
import scala.util.{ Failure, Success, Try, Random }

import spoker._
import spoker.betting._
import spoker.dealer._
import spoker.hand._

object Table {
  def apply(
    players: Seq[PositionedPlayer],
    blinds: Blinds = Blinds(smallBlind = 1, bigBlind = 2),
    cardsDealing: CardsDealing = CardsDealing): Table = {
    val cardsManagement = new CardsManagement
    new Table(
      currentRound = None,
      players = players.map(p =>
        ManageablePlayer(
          positionedPlayer = p,
          cardsManagement = cardsManagement)),
      cardsDealing = cardsDealing,
      cardsManagement = cardsManagement,
      stackManager = new StackManager(players, blinds))
  }
}

case class Table(
  currentRound: Option[BettingRound],
  players: Seq[ManageablePlayer],
  cardsDealing: CardsDealing,
  cardsManagement: CardsManagement,
  stackManager: StackManager)
  extends AnyRef with Dealer with PlayersPositioning {

  def newHand = {
    val table = copy(
      currentRound = Some(BettingRound.preFlop(
        players = players,
        bettersToAct = bettersToAct.startingToTheLeftOfBigBlind,
        smallBlind = smallBlind,
        bigBlind = bigBlind,
        stackManager = stackManager)))
    table.dealHoleCards
    table
  }

  def nextRound = {
    if (!currentRound.get.hasEnded)
      throw new UnclosedRoundException
      
    currentRound.get.kind match {
      case River => throw new NoMoreRoundsException
      case PreFlop => dealFlopCards
      case _ => dealNextCommunityCard
    }
    
    stackManager.pushTableStacksToPot
    
    val newBettersToAct: Seq[ManageablePlayer] = this.startingToTheLeftOfButton.filter(_.status == PlayerStatus.Active)

    copy(
      currentRound = Some(BettingRound.nextRound(
        kind = RoundKind(1 + currentRound.get.kind.id),
        players = this.players.filter(_.status == PlayerStatus.Active),
        stackManager = stackManager,
        bettingState = new BettingState(
          bettersToAct = newBettersToAct
          ))))
  }

  def showdown = {
    val communityCards = cardsManagement.currentState(this.communityName)
    val activePlayers = players.filter((player) => player.status == PlayerStatus.Active || player.status == PlayerStatus.AllIn)
    val bestHand: Hand = activePlayers.map(b => Hand(b.cards ++ communityCards)).sorted.last
    val winners = activePlayers.filter((player) => Hand(player.cards ++ communityCards) == bestHand)

    stackManager.pushTableStacksToPot
    stackManager.givePotToWinners(winners.map(_.positionedPlayer))
    this
  }

  def place(ba: BetterAction): Table = {
    def updatePlayers(action: Action): Seq[ManageablePlayer] = action match {
      case Fold => this.players.updated(this.players.indexOf(ba.better), ba.better.folded) 
      case AllIn => this.players.updated(this.players.indexOf(ba.better), ba.better.pushedAllIn)
      case _ => this.players
    }

    def potToWinner(players: Seq[ManageablePlayer]): Unit = players.filter((player) => player.status == PlayerStatus.Active || player.status == PlayerStatus.AllIn) match {
      case winner :: Nil => {
        stackManager.pushTableStacksToPot
        stackManager.givePotToWinners(winner.positionedPlayer :: Nil)
      }
      case _ => ()
    }

    val current = currentRound.get
    
    val (newBettingState, updatedPlayers) = try {
      (current.place(ba), updatePlayers(ba.action))
    } catch {
      case e: NotEnoughtChipsInStackException => {
        (current.place(ba.better.allIn), updatePlayers(AllIn))
      }
      case e: Exception => throw e
    }

    potToWinner(updatedPlayers)
    copy(
      players = updatedPlayers,
      currentRound = Some(currentRound.get.copy(
        players = updatedPlayers,
        bettingState = newBettingState)))
  }
}
