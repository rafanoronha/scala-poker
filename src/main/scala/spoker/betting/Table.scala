package spoker.betting

import scala.collection.mutable.Map
import scala.util.{ Failure, Success, Try }
import spoker._
import spoker.betting.stack.{ Pot, Blinds, MoveStack, StackHolder, StackManagement }
import spoker.dealer.{ Dealer, CardsDealing, CardsManagement }
import spoker.hand.Hand
import scala.util.Random

object Table {
  def apply(
    players: Seq[PositionedPlayer],
    blinds: Blinds = Blinds(smallBlind = 1, bigBlind = 2),
    cardsDealing: CardsDealing = CardsDealing): Table = {
    val tableName = "Table" + Random.nextInt
    StackManagement.startManaging(tableName, tableName +: players.map(_.name))
    CardsManagement.startManaging(tableName, tableName +: players.map(_.name))
    new Table(
      currentRound = None,
      pot = Pot(
        blinds = blinds,
        tableName = tableName),
      players = players.map(_.copy(tableName = tableName)),
      tableName = tableName,
      cardsDealing = cardsDealing)
  }
}

case class Table(
  currentRound: Option[BettingRound],
  pot: Pot,
  players: Seq[PositionedPlayer],
  tableName: String,
  cardsDealing: CardsDealing) extends AnyRef with Dealer with PlayersPositioning {

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
      case Success(_) =>
        copy(
          currentRound = Some(BettingRound.nextRound(
            kind = RoundKind(1 + currentRound.get.kind.id),
            players = this.players.filter(_.isActive),
            pot = this.pot,
            currentBet = Bet(
              bettersToAct = bettersFromPositionedPlayers(
                bettersToAct.startingToTheLeftOfButton).filter(_.isActive).iterator))))
      case Failure(e) => throw e
    }
  }

  def showdown = {
    val winner = players.filter(_.isActive).zip(players.map(b => Hand(b.cards))).sortBy(_._2).reverse.head._1
    MoveStack(pot.stack, from = pot, to = winner)
    this
  }

  def place(ba: BetterAction): Table = {
    def foldingOut: Seq[PositionedPlayer] =
      if (ba.action == Fold)
        this.players.updated(this.players.indexOf(ba.better.positionedPlayer),
          ba.better.positionedPlayer.copy(isActive = false))
      else this.players
    def potToWinner(players: Seq[PositionedPlayer]): Unit = players.filter(_.isActive) match {
      case winner :: Nil => MoveStack(pot.stack, from = pot, to = winner)
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
