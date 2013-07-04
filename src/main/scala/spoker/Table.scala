package spoker

import spoker.betting._
import spoker.betting.RoundKind.River
import scala.util.{Failure, Success, Try}

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
        case (Some(it)) => (playersFromBetters(it.betters) filter {
          it.contenders contains _
        }, RoundKind(1 + it.kind.id))
      }) match {
        case Success((betters, kind)) => {
          val round = BettingRound(positionedPlayers(betters), kind)
          new Table(players, Some(round))
        }
        case Failure(e) => throw e
      }
    }

    def showdown = Unit

    def betters = currentRound match {
      case (Some(it)) => it.betters
      case _ => Nil
    }

    def place(a: Action) = new Table(players, Some(currentRound.get.place(a)))

    private def positionedPlayers(players: Seq[Player]) = players match {
      case (sb :: bb :: others) =>
        new PositionedPlayer(sb) :: new PositionedPlayer(bb, BigBlind) :: others.map(new PositionedPlayer(_))
    }
  }

  class Player

  object Position extends Enumeration {
    val BigBlind, Any = Value
  }

  val BigBlind = Position.BigBlind

  class PositionedPlayer(val player: Player, val position: Position.Value = Position.Any)

  class Pot(val contenders: Seq[Player]) {
    def gaveUp = (player: Player) => new Pot(contenders.diff(player :: Nil))
  }

  class UnclosedRoundException extends Exception

  class NoMoreRoundsException extends Exception

}
