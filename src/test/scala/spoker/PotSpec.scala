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

class PotSpec extends FunSpec with Matchers with BeforeAndAfter {
  
  type StubbedCards = scala.collection.mutable.Set[Card]

  class StubbedCardsDealing(stub: Map[DealtCardTarget, StubbedCards]) extends CardsDealing {
    override def nextCardTo(target: DealtCardTarget): Card = {
      val card = stub(target).head
      stub(target) -= card
      card
    }
  }

  val StubbedCards = scala.collection.mutable.Set

  def player(name: String)(implicit table: Table) : Better = table.currentRound.get.players.find(
    _.positionedPlayer.name equals name).get


  def player1(implicit table: Table) = player("p1")

  def player2(implicit table: Table) = player("p2")

  def player3(implicit table: Table) = player("p3")


  describe("Split pot") {
    it("should be divided to equal part if possible") {
      implicit val table: Table = Table(
        players = new PositionedPlayer(new Player("p1"), initialStack = 50) ::
          new PositionedPlayer(new Player("p2"), initialStack = 50) ::
          new PositionedPlayer(new Player("p3"), initialStack = 50, isButton = true) :: Nil,
        cardsDealing = new StubbedCardsDealing(
          Map(
            Community -> StubbedCards((Eight, Diamonds), (Seven, Clubs), (Four, Clubs), (Five, Clubs), (Six, Clubs)),
            PlayerReceivingCard("p1") -> StubbedCards((Nine, Hearts), (Ten, Hearts)),
            PlayerReceivingCard("p2") -> StubbedCards((Nine, Spades), (Ten, Spades)),
            PlayerReceivingCard("p3") -> StubbedCards((Two, Hearts), (Two, Spades)))))
        .newHand
        
      val table2 = table.place(player3.call).place(player1.call).place(player2.check)
        .nextRound.place(player1.bet(10)).place(player2.call).place(player3.fold)
        .nextRound.place(player1.bet(20)).place(player2.call)
      table2.showdown

      table.stackManager.getPlayerStack(player1) should be(51)
      table.stackManager.getPlayerStack(player2) should be(51)
      table.stackManager.getPlayerStack(player3) should be(48)
    }
    
    it("should give an extra chip to the first player if the pot can't be divided equally") {
      implicit val table: Table = Table(
        players = new PositionedPlayer(new Player("p1"), initialStack = 50) ::
          new PositionedPlayer(new Player("p2"), initialStack = 50) ::
          new PositionedPlayer(new Player("p3"), initialStack = 50, isButton = true) :: Nil,
        cardsDealing = new StubbedCardsDealing(
          Map(
            Community -> StubbedCards((Eight, Diamonds), (Seven, Clubs), (Four, Clubs), (Five, Clubs), (Six, Clubs)),
            PlayerReceivingCard("p1") -> StubbedCards((Nine, Hearts), (Ten, Hearts)),
            PlayerReceivingCard("p2") -> StubbedCards((Nine, Spades), (Ten, Spades)),
            PlayerReceivingCard("p3") -> StubbedCards((Two, Hearts), (Two, Spades)))))
        .newHand
        
      val table2 = table.place(player3.raise(3)).place(player1.call).place(player2.call)
        .nextRound.place(player1.bet(10)).place(player2.call).place(player3.fold)
        .nextRound.place(player1.bet(20)).place(player2.call)
      table2.showdown

      table.stackManager.getPlayerStack(player1) should be(52)
      table.stackManager.getPlayerStack(player2) should be(51)
      table.stackManager.getPlayerStack(player3) should be(47)
    }
    
  }
}