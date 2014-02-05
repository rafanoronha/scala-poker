package spoker.betting.spec

import org.scalatest.{ BeforeAndAfter, FunSpec }
import org.scalatest.Matchers
import spoker._
import spoker.betting._
import spoker.dealer.CardsDealing
import spoker.dealer.DealtCardTarget
import spoker.dealer.PlayerReceivingCard
import spoker.dealer.Community
import spoker.betting.StackManager

class StackManagerSpec extends FunSpec with Matchers with BeforeAndAfter {

  val player1 = new PositionedPlayer(new Player("p1"), initialStack = 50)
  val player2 = new PositionedPlayer(new Player("p2"), initialStack = 50)
  val player3 = new PositionedPlayer(new Player("p3"), initialStack = 50, isButton = true)
  
  var stackManager: StackManager = null

  before {
    stackManager = new StackManager(players = player1 :: player2 :: player3 :: Nil, blinds = Blinds(smallBlind = 1, bigBlind = 2))
  }

  describe("StackManager") {
    it("should put player's initial stack to player stack") {
      stackManager.getPlayerStack(player1) should be(50)
    }
    
    it("should move chips from player's stack to table stack when betting") {
      stackManager.putChipsOnTable(player1, 5)
      stackManager.getPlayerStack(player1) should be(45)
      stackManager.getTableStack(player1) should be(5)
    }
    
    it("should move chips from table stacks to the pot at the end of round") {
      stackManager.putChipsOnTable(player1, 5)
      stackManager.putChipsOnTable(player2, 5)
      stackManager.pushTableStacksToPot
      stackManager.getTableStack(player1) should be(0)
      stackManager.pots.head.potAmount should be(10)
    }
    
    it("should give a pot to the winner") {
      stackManager.putChipsOnTable(player1, 5)
      stackManager.putChipsOnTable(player2, 5)
      stackManager.pushTableStacksToPot
      stackManager.givePotToWinners(player2 :: Nil)
      stackManager.getPlayerStack(player2) should be(55)
      stackManager.pots.head.potAmount should be(0)
    }
    
    it("should split a pot if there are multiple winners") {
      stackManager.putChipsOnTable(player1, 4)
      stackManager.putChipsOnTable(player2, 4)
      stackManager.putChipsOnTable(player3, 4)
      stackManager.pushTableStacksToPot
      stackManager.pushTableStacksToPot
      stackManager.givePotToWinners(player2 :: player3 :: Nil)
      stackManager.getPlayerStack(player2) should be(52)
      stackManager.getPlayerStack(player3) should be(52)
    }
    
    it("should be able to split non-divisible pot") {
      stackManager.putChipsOnTable(player1, 5)
      stackManager.putChipsOnTable(player2, 5)
      stackManager.putChipsOnTable(player3, 5)
      stackManager.pushTableStacksToPot
      stackManager.pushTableStacksToPot
      stackManager.givePotToWinners(player2 :: player3 :: Nil)
      stackManager.getPlayerStack(player2) should be(53)
      stackManager.getPlayerStack(player3) should be(52)
    }
    
    it("should create side pots"){pending}
  }
}