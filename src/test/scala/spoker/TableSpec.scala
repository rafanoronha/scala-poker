package spoker.table.spec

import org.scalatest.{ BeforeAndAfter, FunSpec }
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Tag
import spoker._
import spoker.betting._
import spoker.dealer.CardsDealing
import spoker.dealer.DealtCardTarget
import spoker.dealer.PlayerReceivingCard
import spoker.dealer.Community

class TableSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  type StubbedCards = scala.collection.mutable.Set[Card]

  class StubbedCardsDealing(stub: Map[DealtCardTarget, StubbedCards]) extends CardsDealing {
    override def nextCardTo(target: DealtCardTarget): Card = {
      println()
      println(target)
      val card = stub(target).head
      stub(target) -= card
      card
    }
  }

  val StubbedCards = scala.collection.mutable.Set

  var table: Table = null

  before {
    table = Table(
      players = new Player("p1") :: new Player("p2") :: new Player("p3") :: Nil,
      cardsDealing = new StubbedCardsDealing(
        Map(
          Community -> StubbedCards((Two, Clubs), (Three, Clubs), (Four, Clubs), (Five, Clubs), (Six, Clubs)),
          PlayerReceivingCard("p1") -> StubbedCards((Nine, Hearths), (Ten, Hearths)),
          PlayerReceivingCard("p2") -> StubbedCards((Nine, Spades), (Ten, Spades)),
          PlayerReceivingCard("p3") -> StubbedCards((Two, Hearths), (Two, Spades)))))
      .newHand
  }

  def player(name: String) = table.currentRound.get.betters.find(
    _.positionedPlayer.name equals name).get

  def player1 = player("p1")

  def player2 = player("p2")

  def player3 = player("p3")

  def playNextRound = {
    table = table.place(player1.call).place(player2.check).place(player3.call)
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
      playNextRound
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
      playNextRound.playNextRound.playNextRound.playNextRound
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
      table = table.place(player1.raise(4)).place(player2.call).place(player3.call)
        .nextRound.place(player1.raise(8)).place(player2.fold).place(player3.call)
        .nextRound.place(player1.raise(8)).place(player3.call)
      table.showdown
      (player3 stack) should be(78)
    }
    it("should be won by unmatched bet owner") {
      table = table.place(player1.raise(4)).place(player2.call).place(player3.fold)
        .nextRound.place(player1.raise(8)).place(player2.fold)
      (player1 stack) should be(56)
    }
    it("should collect the blinds") {
      table.pot.stack should be(3)
    }
    it("should collect all played stakes") {
      table = table.place(player1.raise(4)).place(player2.call).place(player3.fold)
        .nextRound.place(player1.raise(4)).place(player2.call)
        .nextRound.place(player1.raise(4)).place(player2.call)
      table.pot.stack should be(28)
    }
  }

}