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
      def initialState(holderName: String): Int =
        if (holderName == "Pot") 0
        else 50
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
    object UnclosedRound {
      def unapply(round: Some[BettingRound]): Boolean = round.map(!_.hasEnded).getOrElse(false)
    }
    Try((currentRound, currentRound.get.kind) match {
      case (UnclosedRound(), _) => throw new UnclosedRoundException
      case (_, River) => throw new NoMoreRoundsException
      case (_, PreFlop) => dealFlopCards
      case _ => dealNextCommunityCard
    }) match {
      case Success(_) => {
        val bta: Seq[ManageablePlayer] = bettersToAct.startingToTheLeftOfButton.filter(_.isActive)
        copy(
          currentRound = Some(BettingRound.nextRound(
            kind = RoundKind(1 + currentRound.get.kind.id),
            players = this.players.filter(_.isActive),
            pot = this.pot,
            currentBet = Bet(
              bettersToAct = bta.iterator))))
      }
      case Failure(e) => throw e
    }
  }

  def showdown = {
    val winner = players.filter(_.isActive).zip(players.map(b => Hand(b.cards))).sortBy(_._2).reverse.head._1
    winner.collect(pot.stack)(from = pot)
    this
  }

  def place(ba: BetterAction): Table = {
    def foldingOut: Seq[ManageablePlayer] =
      if (ba.action == Fold)
        this.players.updated(this.players.indexOf(ba.better),
          ba.better.folded)
      else this.players
    def potToWinner(players: Seq[ManageablePlayer]): Unit = players.filter(_.isActive) match {
      case winner :: Nil => winner.collect(pot.stack)(from = pot)
      case _ => ()
    }
    val current = currentRound.get
    val bet = current.place(ba)
    val updatedPlayers = foldingOut
    potToWinner(updatedPlayers)
    copy(
      players = updatedPlayers,
      currentRound = Some(currentRound.get.copy(
        players = updatedPlayers,
        currentBet = bet)))
  }
}
