package spoker.table.spec

import org.scalatest.{ BeforeAndAfter, FunSpec }
import org.scalatest.Matchers
import spoker._
import spoker.betting._
import spoker.dealer.CardsDealing
import spoker.dealer.DealtCardTarget
import spoker.dealer.PlayerReceivingCard
import spoker.dealer.Community

class TableSpec extends FunSpec with Matchers with BeforeAndAfter {

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

  before {
    table = Table(
      players = new PositionedPlayer(new Player("p1"), initialStack = 50) ::
        new PositionedPlayer(new Player("p2"), initialStack = 50) ::
        new PositionedPlayer(new Player("p3"), initialStack = 50, isButton = true) :: Nil,
      cardsDealing = new StubbedCardsDealing(
        Map(
          Community -> StubbedCards((Two, Clubs), (Three, Clubs), (Four, Clubs), (Five, Clubs), (Six, Clubs)),
          PlayerReceivingCard("p1") -> StubbedCards((Nine, Hearts), (Ten, Hearts)),
          PlayerReceivingCard("p2") -> StubbedCards((Nine, Spades), (Ten, Spades)),
          PlayerReceivingCard("p3") -> StubbedCards((Two, Hearts), (Two, Spades)))))
      .newHand
  }

  def player(name: String) : Better = table.currentRound.get.players.find(
    _.positionedPlayer.name equals name).get


  def player1 = player("p1")

  def player2 = player("p2")

  def player3 = player("p3")
  
  def playPreFlop = {
    table = table.place(player3.call).place(player1.call).place(player2.check)
    table = table.currentRound.get.kind match {
      case River => table
      case _ => table.nextRound
    }
    this
  }  

  def playNextRound = {
    table = table.place(player1.check).place(player2.check).place(player3.check)
    table = table.currentRound.get.kind match {
      case River => table
      case _ => table.nextRound
    }
    this
  }

  describe("Table") {
    it("should run a hand through pre-flop to showdown") {
      def currentRoundKind = table.currentRound.get.kind
      currentRoundKind should be(PreFlop)
      playPreFlop
      currentRoundKind should be(Flop)
      playNextRound
      currentRoundKind should be(Turn)
      playNextRound
      currentRoundKind should be(River)
      playNextRound
      table.showdown
    }
    it("should not open next round before closing current one") {
      evaluating {
        table.nextRound
      } should produce[UnclosedRoundException]
    }
    it("should not have any betting round after river") {
      playPreFlop.playNextRound.playNextRound.playNextRound
      evaluating {
        table.nextRound
      } should produce[NoMoreRoundsException]
    }
    it("should not open next hand before closing current one")(pending)
  }

  describe("Showdown") {
    it("should only take place after the river")(pending)
  }

  describe("Pot") {
    it("should be won by best showdown rank owner") {
      table = table.place(player3.call).place(player1.call).place(player2.check)
        .nextRound.place(player1.bet(10)).place(player2.fold).place(player3.call)
        .nextRound.place(player1.check).place(player3.check)
      table.showdown
      table.stackManager.getPlayerStack(player3) should be(64)
      table.stackManager.getPlayerStack(player2) should be(48)
      table.stackManager.getPlayerStack(player1) should be(38)
    }
    it("should be won by unmatched bet owner") {
      table = table.place(player3.raise(4)).place(player1.fold).place(player2.call)
        .nextRound.place(player2.check).place(player3.bet(8)).place(player2.fold)
      table.stackManager.getPlayerStack(player3) should be(55)
      table.stackManager.getPlayerStack(player2) should be(46)
      table.stackManager.getPlayerStack(player1) should be(49)
    }
  }
}