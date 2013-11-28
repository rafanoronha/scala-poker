package spoker.betting

import scala.util.{Failure, Success, Try}
import spoker._
import spoker.betting.stack.{Pot, Blinds, MoveStack, StackHolder, StackHolderChief, UpdatedStackReport}

object Table {
  def apply(
    players: Seq[Player],
    blinds: Blinds = Blinds(smallBlind = 1, bigBlind = 2)): Table =
    new Table(
      players = players,
      currentRound = None,
      p = Pot(
        blinds = blinds),
      bsOption = None)
}

class Table(
  val players: Seq[Player],
  var currentRound: Option[BettingRound],
  p: Pot,
  bsOption: Option[Seq[PositionedPlayer]]) extends StackHolderChief {

  var pot: Pot = p.withTable(this)
  var bs: Option[Seq[PositionedPlayer]] = bsOption

  def report(data: UpdatedStackReport): Unit = data match {
    case (p: Pot, updatedStack) => updatePotStack(p, updatedStack)
    case (pp: PositionedPlayer, updatedStack) => updateBetterStack(pp, updatedStack)
  }

  def updatePotStack(pot: Pot, updatedStack: Int): Unit =
    this.pot = this.pot.copy(stack = this.pot.stack + updatedStack)

  def updateBetterStack(better: PositionedPlayer, updatedStack: Int): Unit = {
    val xs = bs.get
    val i = xs.indexOf(better)
    this.bs = Some(xs.updated(i, xs(i).copy(stack = xs(i).stack + updatedStack)))
  }

  def newHand = {
    this.bs = Some(positionPlayers(players))
    this.currentRound = Some(BettingRound.preFlop(
      bs = bs.get,
      pot = pot))
    this
  }

  def betters: Seq[PositionedPlayer] = bs.getOrElse(Nil)

  def nextRound = {
    object UnclosedRound {
      def unapply(round: Some[BettingRound]) = round.isDefined && !round.get.hasEnded
    }
    object RiverRound {
      def unapply(round: Option[BettingRound]) = round.isDefined && River == round.get.kind
    }
    Try(currentRound match {
      case UnclosedRound() => throw new UnclosedRoundException
      case RiverRound() => throw new NoMoreRoundsException
      case (Some(it)) => RoundKind(1 + it.kind.id)
    }) match {
      case Success(kind) => {
        val round = BettingRound.nextRound(
          kind = kind,
          bs = this.betters,
          pot = this.pot,
          currentBet = this.currentRound.get.currentBet.copy(
            bettersToAct = bettersFromPositionedPlayers(this.betters).iterator))
        this.currentRound = Some(round)
        this
      }
      case Failure(e) => throw e
    }
  }

  def showdown = Unit

  def place(ba: BetterAction): Table = {
    val current = currentRound.get
    val bet = current.place(ba)
    if (ba.action == Fold) {
      this.bs = Some(this.betters.diff(ba.better :: Nil))
    }
    val next = current.copy(
      bs = this.betters,
      currentBet = bet)
    if (1 == this.betters.size) {
      MoveStack(pot.stack, from = pot, to = this.betters.head)
    }
    this.currentRound = Some(next)
    this
  }

  private def positionPlayers(players: Seq[Player]) = players match {
    case Nil => Nil
    case (sb :: bb :: others) =>
      new PositionedPlayer(sb, this, SmallBlind) ::
        new PositionedPlayer(bb, this, BigBlind) ::
        others.map(p => new PositionedPlayer(p, this))
  }
}
