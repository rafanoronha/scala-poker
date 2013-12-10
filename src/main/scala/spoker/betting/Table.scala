package spoker.betting

import scala.collection.mutable.Map
import scala.util.{ Failure, Success, Try }
import spoker._
import spoker.betting.stack.{ Pot, Blinds, MoveStack, StackHolder, StackManagement, TableStatus }

object Table {
  def apply(
    players: Seq[Player],
    blinds: Blinds = Blinds(smallBlind = 1, bigBlind = 2)): Table = {
    val tableName = "Table"
    def startStackManagement = {
      val (potStatus: TableStatus, playersStatus: TableStatus) =
        (Map(tableName -> 0), players.foldLeft(Map[String, Int]())((status, player) => status + (player.name -> 50)))
      StackManagement.startManaging(tableName, potStatus ++ playersStatus)
    }
    startStackManagement
    new Table(
      players = players,
      currentRound = None,
      pot = Pot(
        blinds = blinds,
        tableName = tableName),
      betters = None,
      name = tableName)
  }
}

case class Table(
  players: Seq[Player],
  currentRound: Option[BettingRound],
  pot: Pot,
  betters: Option[Seq[PositionedPlayer]],
  name: String) {

  def newHand = {
    val updatedBetters = positionPlayers(players)
    copy(
      betters = Some(updatedBetters),
      currentRound = Some(BettingRound.preFlop(
        betters = updatedBetters,
        pot = pot)))
  }

  def nextRound = {
    object UnclosedRound {
      def unapply(round: Some[BettingRound]): Boolean = round.map(!_.hasEnded).getOrElse(false)
    }
    object RiverRound {
      def unapply(round: Some[BettingRound]) = round.map(_.kind == River).getOrElse(false)
    }
    Try(currentRound match {
      case UnclosedRound() => throw new UnclosedRoundException
      case RiverRound() => throw new NoMoreRoundsException
      case (Some(it)) => RoundKind(1 + it.kind.id)
    }) match {
      case Success(kind) =>
        copy(currentRound = Some(BettingRound.nextRound(
          kind = kind,
          betters = this.betters.get,
          pot = this.pot,
          currentBet = this.currentRound.get.currentBet.copy(
            bettersToAct = bettersFromPositionedPlayers(this.betters.get).iterator))))
      case Failure(e) => throw e
    }
  }

  def showdown = Unit

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
      new PositionedPlayer(sb, this.name, SmallBlind) ::
        new PositionedPlayer(bb, this.name, BigBlind) ::
        others.map(p => new PositionedPlayer(p, this.name))
  }
}
