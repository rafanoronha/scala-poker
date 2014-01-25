package spoker

import scala.collection.mutable.Map
import scala.util.{ Failure, Success, Try, Random }

import spoker._
import spoker.betting._
import spoker.betting.stack._
import spoker.dealer._
import spoker.hand._

object Table {
  def apply(
    players: Seq[PositionedPlayer],
    blinds: Blinds = Blinds(smallBlind = 1, bigBlind = 2),
    cardsDealing: CardsDealing = CardsDealing): Table = {
    val cardsManagement = new CardsManagement
    val stackManagement = new StackManagement {
      def initialState(holderName: String): Double =
        if (holderName == "Pot") 0
        else players.find(_.name == holderName).get.initialStack
    }
    new Table(
      currentRound = None,
      pot = Pot(
        blinds = blinds,
        stackManagement = stackManagement),
      players = players.map(p =>
        ManageablePlayer(
          positionedPlayer = p,
          cardsManagement = cardsManagement,
          stackManagement = stackManagement)),
      cardsDealing = cardsDealing,
      cardsManagement = cardsManagement,
      stackManagement = stackManagement)
  }
}

case class Table(
  currentRound: Option[BettingRound],
  pot: Pot,
  players: Seq[ManageablePlayer],
  cardsDealing: CardsDealing,
  cardsManagement: CardsManagement,
  stackManagement: StackManagement)
  extends AnyRef with Dealer with PlayersPositioning {

  def newHand = {
    val table = copy(
      currentRound = Some(BettingRound.preFlop(
        players = players,
        bettersToAct = bettersToAct.startingToTheLeftOfBigBlind,
        smallBlind = smallBlind,
        bigBlind = bigBlind,
        pot = pot)))
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
    
    val newBettersToAct: Seq[ManageablePlayer] = this.startingToTheLeftOfButton.filter(better => better.isActive && !better.isAllIn)

    copy(
      currentRound = Some(BettingRound.nextRound(
        kind = RoundKind(1 + currentRound.get.kind.id),
        players = this.players.filter(_.isActive),
        pot = this.pot,
        bettingState = BettingState(
          bettersToAct = newBettersToAct))))
  }

  def showdown = {
    val winner = players.filter(_.isActive).zip(players.map(b => Hand(b.cards))).sortBy(_._2).reverse.head._1
    winner.collect(pot.stack)(from = pot)
    this
  }

  def place(ba: BetterAction): Table = {
    val current = currentRound.get
    val bettingState = current.place(ba)

    def updatePlayers: Seq[ManageablePlayer] = ba.action match {
      case Fold => this.players.updated(this.players.indexOf(ba.better), ba.better.folded)
      case _ => 
        if (!bettingState.bettersActed.map(_.manageablePlayer.name).contains(ba.better.manageablePlayer.name))
          this.players.updated(this.players.indexOf(ba.better), ba.better.pushedAllIn)
        else
          this.players
    }

    def potToWinner(players: Seq[ManageablePlayer]): Unit = players.filter(_.isActive) match {
      case winner :: Nil => winner.collect(pot.stack)(from = pot)
      case _ => ()
    }
    val updatedPlayers = updatePlayers
    potToWinner(updatedPlayers)
    copy(
      players = updatedPlayers,
      currentRound = Some(currentRound.get.copy(
        players = updatedPlayers,
        bettingState = bettingState)))
  }
}
