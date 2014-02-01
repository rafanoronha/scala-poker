package spoker

package object betting {

  type BetterActionTuple = (Action, Better)

  object RoundKind extends Enumeration {
    val PreFlop, Flop, Turn, River = Value
  }

  object BetterAction {
    def apply(better: Better) = {
      (a: Action) => new BetterAction(a, better)
    }
  }

  case class BetterAction(action: Action, better: Better)

  class OutOfTurnException extends Exception

  class NonBigBlindPreFlopCheckException extends Exception

  class CantCheckException extends Exception

  class CantBetException extends Exception

  class CantCallException extends Exception

  class CantRaiseException extends Exception

  class NotEnoughtChipsInStackException extends Exception

  class UnclosedRoundException extends Exception

  class NoMoreRoundsException extends Exception

  val PreFlop = RoundKind.PreFlop
  val Flop = RoundKind.Flop
  val Turn = RoundKind.Turn
  val River = RoundKind.River

  implicit def betterFromManageablePlayer(manageablePlayer: ManageablePlayer): Better =
    Better(manageablePlayer)

  implicit def bettersFromManageablePlayers(manageablePlayers: Seq[ManageablePlayer]): Seq[Better] =
    manageablePlayers map betterFromManageablePlayer

  implicit def manageablePlayerFromBetter(better: Better): ManageablePlayer =
    better.manageablePlayer

  implicit def positionedPlayerFromBetter(b: Better): PositionedPlayer =
    b.positionedPlayer

  implicit def positionedPlayerFromManageablePlayer(mp: ManageablePlayer): PositionedPlayer =
    mp.positionedPlayer

  implicit def playerFromBetter(better: Better): Player =
    better.manageablePlayer.positionedPlayer.player

  implicit def fromTuple(tuple: BetterActionTuple): BetterAction = BetterAction(tuple._1, tuple._2)

}