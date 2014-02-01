package spoker.betting

import spoker.`package`.Player

case class Blinds(smallBlind: Double, bigBlind: Double)

class StackManager(players: Seq[PositionedPlayer], blinds: Blinds) {
  
  class Pot (open: Boolean = true) {
    var potAmount: Double = 0
    var players: Set[String] = Set.empty
    
    def addStack(playerName: String, amount: Double) {
      potAmount = potAmount + amount
      players = players + playerName
    }
  }
  
  var currentBet: Option[Double] = None
  var betPlacedBy: Option[PositionedPlayer] = None
  
  private var playerStacks: Map[String, Double] =
    players.map(player => player.name -> player.initialStack).toMap
  
  private def emptyStacks = players.map(player => player.name -> 0.0).toMap
  
  private var tableStacks: Map[String, Double] = emptyStacks
    
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

  def bet(player: PositionedPlayer, amount: Double) {
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
  
  def raise(player: PositionedPlayer, amount: Double) {
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
  
  def putChipsOnTable(player: PositionedPlayer, amount: Double) {
    if (amount >= playerStacks(player.name)) 
        throw new NotEnoughtChipsInStackException
    putChipsOnTableWithoutAllInCheck(player, amount)
  }
  
  private def putChipsOnTableWithoutAllInCheck(player: PositionedPlayer, amount: Double) {
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
  
  def givePotToWinner(player: PositionedPlayer) {
    playerStacks = playerStacks.updated(player.name, playerStacks(player.name) + pots.head.potAmount)
    pots = new Pot() :: Nil
  }
  
  def getPlayerStack(player: PositionedPlayer): Double = playerStacks(player.name)
  def getPlayerStack(player: String): Double = playerStacks(player)

  def getTableStack(player: PositionedPlayer): Double = tableStacks(player.name)
  def getTableStack(player: String): Double = tableStacks(player)
  
  override def toString: String = {
    val playerStackString = playerStacks.keys.map((player) => player + ": " + playerStacks(player)).mkString(", ")
    val tableStackString = tableStacks.keys.map((player) => player + ": " + tableStacks(player)).mkString(", ")
    val potString = pots.map(_.potAmount).mkString(", ")
    "PlayerStacks: " + playerStackString + "\nTableStacks: " + tableStackString + "\nPots: " + potString
  }
}