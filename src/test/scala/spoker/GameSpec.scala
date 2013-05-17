package spoker.game

import spoker.cards.Rank._
import spoker.cards.Suit._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class GameSpec extends FunSpec with ShouldMatchers {

  it("should understand royal flush") {
    Game(Seq((Ace,Clubs),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))).isInstanceOf[RoyalFlush].should(be(true))
  }
  it("should understand a game irrespective of order of the cards") {
    Game(Seq((Ace,Clubs),(Queen,Clubs),(King,Clubs),(Ten,Clubs),(Jack,Clubs))).isInstanceOf[RoyalFlush].should(be(true))
  }
  it("should understand straight flush") {
    Game(Seq((Six,Diamonds),(Seven,Diamonds),(Eight,Diamonds),(Nine,Diamonds),(Ten,Diamonds))).isInstanceOf[StraightFlush].should(be(true))
  }
  it("should understand four of a kind") {
    Game(Seq((Ace,Spades),(Ace,Hearths),(Ace,Clubs),(Ace,Diamonds),(Three,Clubs))).isInstanceOf[FourOfAKind].should(be(true))
  }
  it("should understand full house") {
    Game(Seq((Three,Spades),(Three,Hearths),(Two,Spades),(Two,Clubs),(Two,Diamonds))).isInstanceOf[FullHouse].should(be(true))
  }
  it("should understand flush") {
    Game(Seq((Seven,Hearths),(Queen,Hearths),(Nine,Hearths),(Two,Hearths),(Five,Hearths))).isInstanceOf[Flush].should(be(true))
  }
  it("should understand broadway straight") {
    Game(Seq((Ace,Spades),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))).isInstanceOf[Straight].should(be(true))
  }
  it("should understand three of a kind") {
    Game(Seq((Three,Spades),(Three,Hearths),(Queen,Hearths),(Ace,Diamonds),(Three,Clubs))).isInstanceOf[ThreeOfAKind].should(be(true))
  }
  it("should understand two pair") {
    Game(Seq((Three,Spades),(Three,Hearths),(Five,Spades),(Two,Clubs),(Two,Diamonds))).isInstanceOf[TwoPair].should(be(true))
  }
  it("should understand one pair") {
    Game(Seq((Three,Spades),(Four,Spades),(Five,Spades),(Six,Clubs),(Six,Diamonds))).isInstanceOf[OnePair].should(be(true))
  }
  it("should understand high card") {
    Game(Seq((Ace,Clubs),(Seven,Diamonds),(Eight,Diamonds),(Nine,Diamonds),(Ten,Diamonds))).isInstanceOf[HighCard].should(be(true))
  }
  it("should be ranked") {
    val actual = Seq(Game(Seq((Ace,Clubs),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))),
      Game(Seq((Three,Spades),(Three,Hearths),(Two,Spades),(Two,Clubs),(Two,Diamonds))),
        Game(Seq((Three,Spades),(Three,Hearths),(Five,Spades),(Two,Clubs),(Two,Diamonds)))).map(_.ranking)
    actual should equal (Seq(GameRanking.RoyalFlush,GameRanking.FullHouse,GameRanking.TwoPair))
  }
  it("should compare different games by ranking") {
    Game(Seq((Ace,Clubs),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))) should
      be > Game(Seq((Six,Diamonds),(Seven,Diamonds),(Eight,Diamonds),(Nine,Diamonds),(Ten,Diamonds)))
    Game(Seq((Ace,Spades),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))) should
      be < Game(Seq((Three,Spades),(Three,Hearths),(Two,Spades),(Two,Clubs),(Two,Diamonds)))
    Game(Seq((Three,Spades),(Three,Hearths),(Five,Spades),(Two,Clubs),(Two,Diamonds))) should
      be > Game(Seq((Three,Spades),(Four,Spades),(Five,Spades),(Six,Clubs),(Six,Diamonds)))
  }
  it("should rank royal flush as greater than straight flush") {
    GameRanking.RoyalFlush should be > GameRanking.StraightFlush
  }
  it("should rank straight flush as greater than four of a kind") {
    GameRanking.StraightFlush should be > GameRanking.FourOfAKind
  }
  it("should rank four of a kind as greater than full house") {
    GameRanking.FourOfAKind should be > GameRanking.FullHouse
  }
  it("should rank full house as greater than flush") {
    GameRanking.FullHouse should be > GameRanking.Flush
  }
  it("should rank flush as greater than straight") {
    GameRanking.Flush should be > GameRanking.Straight
  }
  it("should rank straight as greater than three of a kind") {
    GameRanking.Straight should be > GameRanking.ThreeOfAKind
  }
  it("should rank three of a kind as greater than two pair") {
    GameRanking.ThreeOfAKind should be > GameRanking.TwoPair
  }
  it("should rank two pair as greater than one pair") {
    GameRanking.TwoPair should be > GameRanking.OnePair
  }
  it("should rank one pair as greater than high card") {
    GameRanking.OnePair should be > GameRanking.HighCard
  }
  it("should high card as less than full house") {
    GameRanking.HighCard should be < GameRanking.FullHouse
  }
}
