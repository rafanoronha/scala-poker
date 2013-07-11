package spoker

import spoker.betting._
import spoker.betting.RoundKind.River
import scala.util.{Failure, Success, Try}
import spoker.stack.StackHolder

package object table {

  object Table {
    def apply(players: Seq[Player]) = new Table(players, None)
  }

  class Table private(
                       private val players: Seq[Player],
                       val currentRound: Option[BettingRound]) {

    def newHand = new Table(players, Some(BettingRound(positionedPlayers(players), RoundKind.PreFlop)))

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
        case (Some(it)) => (it.betters.filter(it.contenders.contains(_)
        ), RoundKind(1 + it.kind.id))
      }) match {
        case Success((betters, kind)) => {
          val round = BettingRound(positionedPlayers(betters), kind)
          new Table(players, Some(round))
        }
        case Failure(e) => throw e
      }
    }

    def showdown = Unit

    def betters: Seq[Better] = currentRound match {
      case (Some(it)) =>
        if (winner.isDefined) {
          it.betters.updated(
            it.betters.indexOf(winner.get),
            winner.get.collect(it.pot.stack))
        } else it.betters
      case _ => Nil
    }

    def winner = currentRound match {
      case (Some(it)) =>
        if (it.contenders.tail.isEmpty) Some(it.contenders.head)
        else None
      case _ => None
    }

    def place(a: Action) = new Table(players, Some(currentRound.get.place(a)))

    private def positionedPlayers(players: Seq[Player]) = players match {
      case Nil => Nil
      case (sb :: bb :: others) =>
        new PositionedPlayer(sb) :: new PositionedPlayer(bb,
          BigBlind) :: others.map(new PositionedPlayer(_))
    }
  }

  case class Player(name: String, stack: Int = 50) extends StackHolder[Player] {
    def collect(stack: Int) = copy(stack = this.stack + stack)

    def submit(stack: Int) = copy(stack = this.stack - stack)

    override def equals(that: Any) = that match {
      case p: Player => this.name == p.name
      case _ => false
    }

    override def hashCode = name.##
  }

  object Position extends Enumeration {
    val BigBlind, Any = Value
  }

  val BigBlind = Position.BigBlind

  class PositionedPlayer(p: => Player, val position: Position.Value = Position.Any)
    extends StackHolder[PositionedPlayer] {

    val stack = player.stack

    def player = p

    def collect(stack: Int) = new PositionedPlayer(player.collect(stack), position)

    def submit(stack: Int) = new PositionedPlayer(player.submit(stack), position)
  }

  class UnclosedRoundException extends Exception

  class NoMoreRoundsException extends Exception

}
