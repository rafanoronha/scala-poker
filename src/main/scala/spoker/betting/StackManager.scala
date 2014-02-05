package spoker.betting

import spoker.`package`.Player
import spoker.ManageablePlayer

case class Blinds(smallBlind: Int, bigBlind: Int)

class StackManager(players: Seq[PositionedPlayer], blinds: Blinds) {
  
  class Pot (open: Boolean = true) {
    var potAmount: Int = 0
    var players: Set[String] = Set.empty
    
    def addStack(playerName: String, amount: Int) {
      potAmount = potAmount + amount
      players = players + playerName
    }
  }
  
  var currentBet: Option[Int] = None
  var betPlacedBy: Option[PositionedPlayer] = None
  
  private var playerStacks: Map[String, Int] =
    players.map(player => player.name -> player.initialStack).toMap
  
  private def emptyStacks = players.map(player => player.name -> 0).toMap
  
  private var tableStacks: Map[String, Int] = emptyStacks
    
  var pots: List[Pot] = new Pot() :: Nil

  def collectSmallBlind(player: PositionedPlayer) {
    bet(player, blinds.smallBlind)
  }
  
  def collectBigBlind(player: PositionedPlayer) {
    raise(player, blinds.bigBlind)
  }

  def check(player: PositionedPlayer) {
    if (currentBet != None && betPlacedBy.get != player)
      throw new CantCheckException
  }

  def bet(player: PositionedPlayer, amount: Int) {
    currentBet match {
      case Some(amount) => throw new CantBetException
      case None => {
        putChipsOnTable(player, amount)
        currentBet = Some(amount)
        betPlacedBy = Some(player)
      }
    }
  }
  
  def call(player: PositionedPlayer) {
    currentBet match {
      case None => throw new CantCallException
      case Some(amount) => {
        putChipsOnTable(player, amount - tableStacks(player.name))
      }
    }
  }
  
  def raise(player: PositionedPlayer, amount: Int) {
    currentBet match {
      case None => throw new CantRaiseException
      case Some(betAmount) => {
        putChipsOnTable(player, amount - tableStacks(player.name))
        currentBet = Some(amount)
        betPlacedBy = Some(player)
      }
    }
  }
  
  def allIn(player: PositionedPlayer) {
    val stack = playerStacks(player.name)
    putChipsOnTableWithoutAllInCheck(player, stack)
    
    val allInAmount = tableStacks(player.name)

    currentBet match {
      case Some(betAmount) if betAmount >= allInAmount  => ()
      case _ => {
        currentBet = Some(allInAmount)
        betPlacedBy = Some(player)
      }
    }
  }
  
  def putChipsOnTable(player: PositionedPlayer, amount: Int) {
    if (amount >= playerStacks(player.name)) 
        throw new NotEnoughtChipsInStackException
    putChipsOnTableWithoutAllInCheck(player, amount)
  }
  
  private def putChipsOnTableWithoutAllInCheck(player: PositionedPlayer, amount: Int) {
    playerStacks = playerStacks.updated(player.name, playerStacks(player.name) - amount)
    tableStacks = tableStacks.updated(player.name, tableStacks(player.name) + amount)
  }

  def pushTableStacksToPot {
    for ((player, stack) <- tableStacks) {
      pots.head.addStack(player, stack)
    }
    tableStacks = emptyStacks
    currentBet = None
  }
  
  def givePotToWinners(winners: Seq[PositionedPlayer]) {
    val partOfPot = pots.head.potAmount / winners.size
    for (winner <- winners) {
      playerStacks = playerStacks.updated(winner.name, playerStacks(winner.name) + partOfPot)
    }
    
    val remainderOfPot = pots.head.potAmount - (partOfPot * winners.size)
    for (winner <- winners.take(remainderOfPot)) {
      playerStacks = playerStacks.updated(winner.name, playerStacks(winner.name) + 1)
    }
    
    pots = new Pot() :: Nil
  }
  
  def getPlayerStack(player: PositionedPlayer): Int = playerStacks(player.name)
  def getPlayerStack(player: String): Int = playerStacks(player)

  def getTableStack(player: PositionedPlayer): Int = tableStacks(player.name)
  def getTableStack(player: String): Int = tableStacks(player)
  
  override def toString: String = {
    val playerStackString = playerStacks.keys.map((player) => player + ": " + playerStacks(player)).mkString(", ")
    val tableStackString = tableStacks.keys.map((player) => player + ": " + tableStacks(player)).mkString(", ")
    val potString = pots.map(_.potAmount).mkString(", ")
    "PlayerStacks: " + playerStackString + "\nTableStacks: " + tableStackString + "\nPots: " + potString
  }
}