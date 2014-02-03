package spoker.betting.spec

import org.scalatest.{ BeforeAndAfter, FunSpec }
import org.scalatest.Matchers
import spoker._
import spoker.betting._
import spoker.dealer._

class BettingSpec extends FunSpec with Matchers with BeforeAndAfter {

  type StubbedCards = scala.collection.mutable.Set[Card]

  class StubbedCardsDealing(stub: Map[DealtCardTarget, StubbedCards]) extends CardsDealing {
    override def nextCardTo(target: DealtCardTarget): Card = {
      val card = stub(target).head
      stub(target) -= card
      card
    }
  }

  val StubbedCards = scala.collection.mutable.Set
  
  var table: Table = null

  var round: BettingRound = null

  before {
    table = Table(
      players = new PositionedPlayer(new Player("smallBlind"), initialStack = 1500) ::
        new PositionedPlayer(new Player("bigBlind"), initialStack = 1500) ::
        new PositionedPlayer(new Player("dealer"), initialStack = 1500, isButton = true) :: Nil,
      cardsDealing = new StubbedCardsDealing(
        Map(
          Community -> StubbedCards((Two, Clubs), (Three, Clubs), (Four, Clubs), (Five, Clubs), (Six, Clubs)),
          PlayerReceivingCard("smallBlind") -> StubbedCards((Nine, Hearts), (Ten, Hearts)),
          PlayerReceivingCard("bigBlind") -> StubbedCards((Nine, Spades), (Ten, Spades)),
          PlayerReceivingCard("dealer") -> StubbedCards((Two, Hearts), (Two, Spades))))).newHand
    round = table.currentRound.get
  }

  def player(name: String): Better = table.currentRound.get.players.find(
    _.positionedPlayer.name equals name).get

  def smallBlind = player("smallBlind")

  def bigBlind = player("bigBlind")

  def dealer = player("dealer")

  describe("Rounds") {
    it("should end when all players acted and no open bet exist") {
      round = table.place(dealer.fold).place(smallBlind.raise(10)).place(bigBlind.call).currentRound.get
      round.hasEnded should be(true)
    }
    it("should end when a bet is not matched") {
      round = table.place(dealer.fold).place(smallBlind.raise(10)).place(bigBlind.fold).currentRound.get
      round.hasEnded should be(true)
    }
    it("should not end while an open bet exist") {
      round = table.place(dealer.raise(10)).place(smallBlind.raise(20)).place(bigBlind.fold).currentRound.get
      round.hasEnded should be(false)
    }
  }

  describe("Players") {
    it("should be able to limp in") {
      table.place(dealer.call).place(smallBlind.call).place(bigBlind.check)
    }
    it("should be able to fold an initial bet") {
      table.place(dealer.fold)
    }
    it("should be able to fold a raised bet") {
      table.place(dealer.raise(10)).place(smallBlind.fold)
    }
    it("should be able to push all in") {
      table.place(dealer.allIn).place(smallBlind.allIn).place(bigBlind.allIn)
    }
  }
  describe("Check") {
    it("should not be placed if any betting action got place") {
      round = table.place(dealer.call).place(smallBlind.raise(10)).place(bigBlind.allIn).currentRound.get
      evaluating {
        round.place(dealer.check)
      } should produce[CantCheckException]
    }
  }
  describe("Bet") {
    it("should not be placed if other bet is open") {
      round = table.place(dealer.call).place(smallBlind.raise(10)).place(bigBlind.call).currentRound.get
      evaluating {
        round.place(dealer.bet(5))
      } should produce[CantBetException]
    }
  }
  describe("Raise") {
    it("should not be placed when there is no open bet") {
      round = table.place(dealer.call).place(smallBlind.call).place(bigBlind.check)
        .nextRound.currentRound.get
      evaluating {
        round.place(smallBlind.raise(5))
      } should produce[CantRaiseException]
    }
    it("should not be higher than player's stack") {
      evaluating {
        round.place(dealer.bet(2000))
      } should produce[CantBetException]
    }
    it("should take correct amount from the player's stack in the preflop round") {
      table.place(dealer.raise(8))
      table.stackManager.getPlayerStack("dealer") should equal(1492)
    }
    it("should take correct amount from the player's stack in the flop round") {
      table.place(dealer.raise(8)).place(smallBlind.call).place(bigBlind.call)
        .nextRound.place(smallBlind.bet(2)).place(bigBlind.raise(12))
      table.stackManager.getPlayerStack("bigBlind") should equal(1480)
    }
    it("should take correct amount from the player's stack in the turn round") {
      table.place(dealer.raise(8)).place(smallBlind.call).place(bigBlind.call)
        .nextRound.place(smallBlind.bet(2)).place(bigBlind.raise(12))
        .place(dealer.call).place(smallBlind.call).nextRound
        .place(smallBlind.bet(2)).place(bigBlind.raise(12))
      table.stackManager.getPlayerStack("bigBlind") should equal(1468)
    }
    it("should take correct amount from the player's stack in the river round") {
      table.place(dealer.raise(8)).place(smallBlind.call).place(bigBlind.call)
        .nextRound.place(smallBlind.bet(2)).place(bigBlind.raise(12))
        .place(dealer.call).place(smallBlind.call).nextRound
        .place(smallBlind.bet(2)).place(bigBlind.raise(12))
        .place(dealer.call).place(smallBlind.call).nextRound
        .place(smallBlind.bet(2)).place(bigBlind.raise(12))
      table.stackManager.getPlayerStack("bigBlind") should equal(1456)
    }
  }
  
  describe("All in") {
    it("should steal the blinds if other players fold") {
      table.place(dealer.allIn).place(smallBlind.fold).place(bigBlind.fold)
      table.stackManager.getPlayerStack("dealer") should be(1503)
      table.stackManager.getPlayerStack("smallBlind") should be(1499)
      table.stackManager.getPlayerStack("bigBlind") should be(1498)
    }
    it("should be won by best showdown rank owner") {
      table = table.place(dealer.allIn).place(smallBlind.allIn).place(bigBlind.allIn)
      table = table.nextRound.nextRound
      table.showdown
      table.stackManager.getPlayerStack("dealer") should be(4500)
      table.stackManager.getPlayerStack("smallBlind") should be(0)
      table.stackManager.getPlayerStack("bigBlind") should be(0)
    }
    it("can be done by raising or calling") {
      println("can be done by raising or calling")
      table = table.place(dealer.raise(1500)).place(smallBlind.fold).place(bigBlind.call).nextRound
      table = table.nextRound.nextRound
      table.showdown
      table.stackManager.getPlayerStack("dealer") should be(3001)
      table.stackManager.getPlayerStack("smallBlind") should be(1499)
      table.stackManager.getPlayerStack("bigBlind") should be(0)
    }
    it("can be done by betting") {
      table = table.place(dealer.call).place(smallBlind.call).place(bigBlind.check).nextRound
      table = table.place(smallBlind.bet(1498)).place(bigBlind.fold).place(dealer.call)
      table = table.nextRound.nextRound
      table.showdown
      table.stackManager.getPlayerStack("dealer") should be(3002)
      table.stackManager.getPlayerStack("smallBlind") should be(0)
      table.stackManager.getPlayerStack("bigBlind") should be(1498)
    }
  }
}