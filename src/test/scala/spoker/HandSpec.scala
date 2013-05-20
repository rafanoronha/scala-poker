package spoker.hand.spec

import spoker.cards.Rank._
import spoker.cards.Suit._
import spoker.hand.Hand
import spoker.hand.HandRanking._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class HandSpec extends FunSpec with ShouldMatchers {

  it("should understand royal flush") {
    Hand(Seq((Ace, Clubs), (King, Clubs), (Queen, Clubs), (Jack, Clubs), (Ten, Clubs)))
      .ranking should be(RoyalFlush)
  }
  it("should understand a hand irrespective of order of the cards") {
    Hand(Seq((Ace, Clubs), (Queen, Clubs), (King, Clubs), (Ten, Clubs), (Jack, Clubs)))
      .ranking should be(RoyalFlush)
  }
  it("should understand straight flush") {
    Hand(Seq((Six, Diamonds), (Seven, Diamonds), (Eight, Diamonds), (Nine, Diamonds), (Ten, Diamonds)))
      .ranking should be(StraightFlush)
  }
  it("should understand four of a kind") {
    Hand(Seq((Ace, Spades), (Ace, Hearths), (Ace, Clubs), (Ace, Diamonds), (Three, Clubs)))
      .ranking should be(FourOfAKind)
  }
  it("should understand full house") {
    Hand(Seq((Three, Spades), (Three, Hearths), (Two, Spades), (Two, Clubs), (Two, Diamonds)))
      .ranking should be(FullHouse)
  }
  it("should understand flush") {
    Hand(Seq((Seven, Hearths), (Queen, Hearths), (Nine, Hearths), (Two, Hearths), (Five, Hearths)))
      .ranking should be(Flush)
  }
  it("should understand broadway straight") {
    Hand(Seq((Ace, Spades), (King, Clubs), (Queen, Clubs), (Jack, Clubs), (Ten, Clubs)))
      .ranking should be(Straight)
  }
  it("should understand three of a kind") {
    Hand(Seq((Three, Spades), (Three, Hearths), (Queen, Hearths), (Ace, Diamonds), (Three, Clubs)))
      .ranking should be(ThreeOfAKind)
  }
  it("should understand two pair") {
    Hand(Seq((Three, Spades), (Three, Hearths), (Five, Spades), (Two, Clubs), (Two, Diamonds)))
      .ranking should be(TwoPair)
  }
  it("should understand one pair") {
    Hand(Seq((Three, Spades), (Four, Spades), (Five, Spades), (Six, Clubs), (Six, Diamonds)))
      .ranking should be(OnePair)
  }
  it("should understand high card") {
    Hand(Seq((Ace, Clubs), (Seven, Diamonds), (Eight, Diamonds), (Nine, Diamonds), (Ten, Diamonds)))
      .ranking should be(HighCard)
  }
  it("should be ranked") {
    val actual = Seq(
      Hand(Seq((Ace, Clubs), (King, Clubs), (Queen, Clubs), (Jack, Clubs), (Ten, Clubs))),
      Hand(Seq((Three, Spades), (Three, Hearths), (Two, Spades), (Two, Clubs), (Two, Diamonds))),
      Hand(Seq((Three, Spades), (Three, Hearths), (Five, Spades), (Two, Clubs), (Two, Diamonds))))
      .map(_.ranking)
    actual should equal(Seq(RoyalFlush, FullHouse, TwoPair))
  }
  it("should compare different hands by ranking") {
    val (royalFlush, straightFlush, straight, fullHouse, twoPair, onePair) =
      (Hand(Seq((Ace, Clubs), (King, Clubs), (Queen, Clubs), (Jack, Clubs), (Ten, Clubs))),
        Hand(Seq((Six, Diamonds), (Seven, Diamonds), (Eight, Diamonds), (Nine, Diamonds), (Ten, Diamonds))),
        Hand(Seq((Ace, Spades), (King, Clubs), (Queen, Clubs), (Jack, Clubs), (Ten, Clubs))),
        Hand(Seq((Three, Spades), (Three, Hearths), (Two, Spades), (Two, Clubs), (Two, Diamonds))),
        Hand(Seq((Three, Spades), (Three, Hearths), (Five, Spades), (Two, Clubs), (Two, Diamonds))),
        Hand(Seq((Three, Spades), (Four, Spades), (Five, Spades), (Six, Clubs), (Six, Diamonds))))

    royalFlush should be > straightFlush
    straight should be < fullHouse
    twoPair should be > onePair
  }
  it("should rank royal flush as greater than straight flush") {
    RoyalFlush should be > StraightFlush
  }
  it("should rank straight flush as greater than four of a kind") {
    StraightFlush should be > FourOfAKind
  }
  it("should rank four of a kind as greater than full house") {
    FourOfAKind should be > FullHouse
  }
  it("should rank full house as greater than flush") {
    FullHouse should be > Flush
  }
  it("should rank flush as greater than straight") {
    Flush should be > Straight
  }
  it("should rank straight as greater than three of a kind") {
    Straight should be > ThreeOfAKind
  }
  it("should rank three of a kind as greater than two pair") {
    ThreeOfAKind should be > TwoPair
  }
  it("should rank two pair as greater than one pair") {
    TwoPair should be > OnePair
  }
  it("should rank one pair as greater than high card") {
    OnePair should be > HighCard
  }
  it("should rank high card as less than full house") {
    HighCard should be < FullHouse
  }
}
