package spoker.betting

import scala.collection.mutable.Map
import scala.util.{ Failure, Success, Try }
import spoker._
import spoker.betting.stack.{ Pot, Blinds, MoveStack, StackHolder, StackManagement }
import spoker.dealer.{ Dealer, CardsDealing, CardsManagement }
import spoker.hand.Hand

object Table {
  def apply(
    players: Seq[Player],
    blinds: Blinds = Blinds(smallBlind = 1, bigBlind = 2),
    cardsDealing: CardsDealing = CardsDealing): Table = {
    val tableName = "Table"
    StackManagement.startManaging(tableName, tableName +: players.map(_.name))
    CardsManagement.startManaging(tableName, tableName +: players.map(_.name))
    new Table(
      players = players,
      currentRound = None,
      pot = Pot(
        blinds = blinds,
        tableName = tableName),
      betters = None,
      tableName = tableName,
      cardsDealing = cardsDealing)
  }
}

case class Table(
  players: Seq[Player],
  currentRound: Option[BettingRound],
  pot: Pot,
  betters: Option[Seq[PositionedPlayer]],
  tableName: String,
  cardsDealing: CardsDealing) extends Dealer {

  def newHand = {
    val updatedBetters = positionPlayers(players)
    val table = copy(
      betters = Some(updatedBetters),
      currentRound = Some(BettingRound.preFlop(
        betters = updatedBetters,
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
        copy(currentRound = Some(BettingRound.nextRound(
          kind = RoundKind(1 + currentRound.get.kind.id),
          betters = this.betters.get,
          pot = this.pot,
          currentBet = this.currentRound.get.currentBet.copy(
            bettersToAct = bettersFromPositionedPlayers(this.betters.get).iterator))))
      case Failure(e) => throw e
    }
  }

  def showdown = {
    val winner = betters.get.zip(betters.get.map(b => Hand(b.cards))).sortBy(_._2).reverse.head._1
    MoveStack(pot.stack, from = pot, to = winner)
    this
  }

  def place(ba: BetterAction): Table = {
    def foldingOut =
      if (ba.action == Fold) Some(this.betters.get.diff(ba.better :: Nil))
      else this.betters
    def potToWinner: PartialFunction[Seq[PositionedPlayer], Unit] = {
      case winner :: Nil => MoveStack(pot.stack, from = pot, to = winner)
    }
    val current = currentRound.get
    val bet = current.place(ba)
    val updatedBetters = foldingOut
    updatedBetters collect potToWinner
    copy(
      betters = updatedBetters,
      currentRound = Some(currentRound.get.copy(
        betters = updatedBetters.get,
        currentBet = bet)))
  }

  private def positionPlayers(players: Seq[Player]) = players match {
    case Nil => Nil
    case (sb :: bb :: others) =>
      new PositionedPlayer(sb, this.tableName, SmallBlind) ::
        new PositionedPlayer(bb, this.tableName, BigBlind) ::
        others.map(p => new PositionedPlayer(p, this.tableName))
  }
}
