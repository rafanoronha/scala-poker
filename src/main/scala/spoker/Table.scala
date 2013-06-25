package spoker

import spoker.betting._

package object table {

  object Table {
    def apply(players: Seq[Player]) = new Table(players, None)
  }

  class Table private(
                       private val players: Seq[Player],
                       private val currentRound: Option[BettingRound]) {

    def newHand = this

    def nextRound = {
      if (currentRound.isDefined && !currentRound.get.hasEnded) {
        throw new UnclosedRoundException
      }
      val (kind, betters) = currentRound match {
        case (Some(it)) => (RoundKind(1 + it.kind.id), playersFromBetters(it.betters) filter {
          it.contenders contains _
        })
        case (None) => (RoundKind.PreFlop, players)
      }
      val round = BettingRound(positionedPlayers(betters), kind)
      new Table(players, Some(round))
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

  class Player {
  }

  object Position extends Enumeration {
    val BigBlind, Any = Value
  }

  val BigBlind = Position.BigBlind

  class PositionedPlayer(val player: Player, val position: Position.Value = Position.Any)

  class Pot(val contenders: Seq[Player]) {
    def gaveUp = (player: Player) => new Pot(contenders.diff(player :: Nil))
  }

  class UnclosedRoundException extends Exception

}
