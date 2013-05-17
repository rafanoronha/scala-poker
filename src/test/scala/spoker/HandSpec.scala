package spoker.hand

import spoker.cards.Rank._
import spoker.cards.Suit._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class HandSpec extends FunSpec with ShouldMatchers {

  it("should understand royal flush") {
    Hand(Seq((Ace,Clubs),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))).isInstanceOf[RoyalFlush].should(be(true))
  }
  it("should understand a hand irrespective of order of the cards") {
    Hand(Seq((Ace,Clubs),(Queen,Clubs),(King,Clubs),(Ten,Clubs),(Jack,Clubs))).isInstanceOf[RoyalFlush].should(be(true))
  }
  it("should understand straight flush") {
    Hand(Seq((Six,Diamonds),(Seven,Diamonds),(Eight,Diamonds),(Nine,Diamonds),(Ten,Diamonds))).isInstanceOf[StraightFlush].should(be(true))
  }
  it("should understand four of a kind") {
    Hand(Seq((Ace,Spades),(Ace,Hearths),(Ace,Clubs),(Ace,Diamonds),(Three,Clubs))).isInstanceOf[FourOfAKind].should(be(true))
  }
  it("should understand full house") {
    Hand(Seq((Three,Spades),(Three,Hearths),(Two,Spades),(Two,Clubs),(Two,Diamonds))).isInstanceOf[FullHouse].should(be(true))
  }
  it("should understand flush") {
    Hand(Seq((Seven,Hearths),(Queen,Hearths),(Nine,Hearths),(Two,Hearths),(Five,Hearths))).isInstanceOf[Flush].should(be(true))
  }
  it("should understand broadway straight") {
    Hand(Seq((Ace,Spades),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))).isInstanceOf[Straight].should(be(true))
  }
  it("should understand three of a kind") {
    Hand(Seq((Three,Spades),(Three,Hearths),(Queen,Hearths),(Ace,Diamonds),(Three,Clubs))).isInstanceOf[ThreeOfAKind].should(be(true))
  }
  it("should understand two pair") {
    Hand(Seq((Three,Spades),(Three,Hearths),(Five,Spades),(Two,Clubs),(Two,Diamonds))).isInstanceOf[TwoPair].should(be(true))
  }
  it("should understand one pair") {
    Hand(Seq((Three,Spades),(Four,Spades),(Five,Spades),(Six,Clubs),(Six,Diamonds))).isInstanceOf[OnePair].should(be(true))
  }
  it("should understand high card") {
    Hand(Seq((Ace,Clubs),(Seven,Diamonds),(Eight,Diamonds),(Nine,Diamonds),(Ten,Diamonds))).isInstanceOf[HighCard].should(be(true))
  }
  it("should be ranked") {
    val actual = Seq(Hand(Seq((Ace,Clubs),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))),
      Hand(Seq((Three,Spades),(Three,Hearths),(Two,Spades),(Two,Clubs),(Two,Diamonds))),
        Hand(Seq((Three,Spades),(Three,Hearths),(Five,Spades),(Two,Clubs),(Two,Diamonds)))).map(_.ranking)
    actual should equal (Seq(HandRanking.RoyalFlush,HandRanking.FullHouse,HandRanking.TwoPair))
  }
  it("should compare different hands by ranking") {
    Hand(Seq((Ace,Clubs),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))) should
      be > Hand(Seq((Six,Diamonds),(Seven,Diamonds),(Eight,Diamonds),(Nine,Diamonds),(Ten,Diamonds)))
    Hand(Seq((Ace,Spades),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))) should
      be < Hand(Seq((Three,Spades),(Three,Hearths),(Two,Spades),(Two,Clubs),(Two,Diamonds)))
    Hand(Seq((Three,Spades),(Three,Hearths),(Five,Spades),(Two,Clubs),(Two,Diamonds))) should
      be > Hand(Seq((Three,Spades),(Four,Spades),(Five,Spades),(Six,Clubs),(Six,Diamonds)))
  }
  it("should rank royal flush as greater than straight flush") {
    HandRanking.RoyalFlush should be > HandRanking.StraightFlush
  }
  it("should rank straight flush as greater than four of a kind") {
    HandRanking.StraightFlush should be > HandRanking.FourOfAKind
  }
  it("should rank four of a kind as greater than full house") {
    HandRanking.FourOfAKind should be > HandRanking.FullHouse
  }
  it("should rank full house as greater than flush") {
    HandRanking.FullHouse should be > HandRanking.Flush
  }
  it("should rank flush as greater than straight") {
    HandRanking.Flush should be > HandRanking.Straight
  }
  it("should rank straight as greater than three of a kind") {
    HandRanking.Straight should be > HandRanking.ThreeOfAKind
  }
  it("should rank three of a kind as greater than two pair") {
    HandRanking.ThreeOfAKind should be > HandRanking.TwoPair
  }
  it("should rank two pair as greater than one pair") {
    HandRanking.TwoPair should be > HandRanking.OnePair
  }
  it("should rank one pair as greater than high card") {
    HandRanking.OnePair should be > HandRanking.HighCard
  }
  it("should high card as less than full house") {
    HandRanking.HighCard should be < HandRanking.FullHouse
  }
}
