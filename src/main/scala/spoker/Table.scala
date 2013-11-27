package spoker

import spoker.betting._
import spoker.betting.RoundKind.River
import scala.util.{Failure, Success, Try}
import spoker.stack.{MoveStack, StackHolder, StackHolderChief, UpdatedStackReport}
import spoker.blinds.{BlindsGathering, Blinds}

package object table {

  val SmallBlind = Position.SmallBlind
  val BigBlind = Position.BigBlind

  trait StackManagement extends StackHolderChief {

    def updateBetterStack(better: PositionedPlayer, updatedStack: Int): Unit

    def updatePotStack(pot: Pot, updatedStack: Int): Unit
    def report(data: UpdatedStackReport): Unit = data match {
      case (p: Pot, updatedStack) => updatePotStack(p, updatedStack)
      case (pp: PositionedPlayer, updatedStack) => updateBetterStack(pp, updatedStack)
    }
  }

  object Table {
    def apply(
               players: Seq[Player],
               blinds: Blinds = Blinds(smallBlind = 1, bigBlind = 2)): Table =
      new Table(
        players = players,
        currentRound = None,
        p = Pot(
          blinds = blinds
        ),
        bsOption = None
      )
  }

  class Table(
               val players: Seq[Player],
               var currentRound: Option[BettingRound],
               p: Pot,
               bsOption: Option[Seq[PositionedPlayer]]) extends StackManagement {

    var pot: Pot = p.withTable(this)
    var bs: Option[Seq[PositionedPlayer]] = bsOption

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
        pot = pot
      ))
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
              bettersToAct = bettersFromPositionedPlayers(this.betters).iterator
            )
          )
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
        currentBet = bet
      )
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

  case class Player(name: String)

  object Position extends Enumeration {
    val BigBlind, SmallBlind, Any = Value
  }

  case class Pot(
                  stack: Int = 0,
                  blinds: Blinds,
                  var table: Table = null) extends BlindsGathering {
    def withTable(t: Table): Pot = {
      table = t
      this
    }

    lazy val chief = table
  }

  class PositionedPlayer(
                          p: Player,
                          table: Table,
                          val position: Position.Value = Position.Any,
                          val stack: Int = 50) extends StackHolder {

    def copy(
              p: Player = this.p,
              table: Table = this.table,
              position: Position.Value = this.position,
              stack: Int = this.stack) =
      new PositionedPlayer(
        p = p,
        table = table,
        position = position,
        stack = stack
      )

    val chief = table

    val playerName = p.name

    def player = table.players.find(playerName == _.name).get

    override def equals(that: Any) = that match {
      case pp: PositionedPlayer => this.player == pp.player
    }

    override def hashCode = this.player.##

  }

  class UnclosedRoundException extends Exception

  class NoMoreRoundsException extends Exception

}
