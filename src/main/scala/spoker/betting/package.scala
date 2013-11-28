package spoker

package object betting {

  type BetterActionTuple = (Action, Better)

  object RoundKind extends Enumeration {
    val PreFlop, Flop, Turn, River = Value
  }

  object Position extends Enumeration {
    val BigBlind, SmallBlind, Any = Value
  }

  object BetterAction {
    def apply(better: Better) = {
      (a: Action) => new BetterAction(a, better)
    }
  }

  case class BetterAction(action: Action, better: Better)

  class OutOfTurnException extends Exception

  class NonBigBlindCheckException extends Exception

  class CantCheckException extends Exception

  class UnclosedRoundException extends Exception

  class NoMoreRoundsException extends Exception

  val PreFlop = RoundKind.PreFlop
  val Flop = RoundKind.Flop
  val Turn = RoundKind.Turn
  val River = RoundKind.River

  val SmallBlind = Position.SmallBlind
  val BigBlind = Position.BigBlind
 
  
  implicit def positionedPlayerFroBetter(better: Better): PositionedPlayer =
    better.positionedPlayer

  implicit def betterFromPositionedPlayer(positionedPlayer: PositionedPlayer): Better =
    Better(positionedPlayer)

  implicit def bettersFromPositionedPlayers(positionedPlayers: Seq[PositionedPlayer]): Seq[Better] =
    positionedPlayers map betterFromPositionedPlayer

  implicit def playersFromBetters(betters: Seq[Better]): Seq[Player] = betters map {
    playerFromBetter(_)
  }

  implicit def playerFromBetter(better: Better): Player = better.positionedPlayer.player

  implicit def fromTuple(tuple: BetterActionTuple): BetterAction = BetterAction(tuple._1, tuple._2)

}