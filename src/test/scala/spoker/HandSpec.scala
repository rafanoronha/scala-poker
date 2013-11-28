package spoker.hand.spec

import spoker._
import spoker.hand.Hand
import spoker.hand.HandRanking._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class HandSpec extends FunSpec with ShouldMatchers {

  it("should understand royal flush") {
    val cards = (Nine, Clubs) ::(Ace, Hearths) ::(King, Clubs) ::
      (Ace, Clubs) ::(Jack, Clubs) ::(Queen, Clubs) ::(Ten, Clubs) :: Nil

    val five = (Ace, Clubs) ::(King, Clubs) ::(Queen, Clubs) ::(Jack, Clubs) ::(Ten, Clubs) :: Nil

    Hand(cards) should have(
      'ranking(RoyalFlush),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand straight flush") {
    val cards = (Seven, Diamonds) ::(Nine, Diamonds) ::(Ace, Spades) ::
      (Eight, Diamonds) ::(Ten, Diamonds) ::(Six, Diamonds) ::(Five, Diamonds) :: Nil

    val five = (Ten, Diamonds) ::(Nine, Diamonds) ::(Eight, Diamonds) ::(Seven, Diamonds) ::(Six, Diamonds) :: Nil

    Hand(cards) should have(
      'ranking(StraightFlush),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand four of a kind") {
    val cards = (Four, Spades) ::(Ace, Spades) ::(Ace, Hearths) ::
      (Two, Spades) ::(Ace, Clubs) ::(Ace, Diamonds) ::(Three, Clubs) :: Nil

    val matched = (Ace, Spades) ::(Ace, Hearths) ::(Ace, Clubs) ::(Ace, Diamonds) :: Nil
    val kicker = (Four, Spades)
    val five = matched :+ kicker

    Hand(cards) should have(
      'ranking(FourOfAKind),
      'cards(five),
      'matched(matched)
    )
  }
  it("should understand full house") {
    val cards = (Three, Spades) ::(Nine, Clubs) ::(Ace, Hearths) ::
      (Three, Hearths) ::(Two, Spades) ::(Two, Clubs) ::(Two, Diamonds) :: Nil

    val five = (Two, Spades) ::(Two, Clubs) ::(Two, Diamonds) ::(Three, Spades) ::(Three, Hearths) :: Nil

    Hand(cards) should have(
      'ranking(FullHouse),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand flush") {
    val cards = (Seven, Hearths) ::(Queen, Hearths) ::(Ace, Spades) ::
      (Ace, Hearths) ::(Nine, Hearths) ::(Two, Hearths) ::(Five, Hearths) :: Nil

    val five = (Ace, Hearths) ::(Queen, Hearths) ::(Nine, Hearths) ::(Seven, Hearths) ::(Five, Hearths) :: Nil

    Hand(cards) should have(
      'ranking(Flush),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand broadway straight") {
    val cards = (Ace, Spades) ::(King, Clubs) ::(Four, Hearths) ::
      (Four, Spades) ::(Queen, Clubs) ::(Jack, Clubs) ::(Ten, Clubs) :: Nil

    val five = (Ace, Spades) ::(King, Clubs) ::(Queen, Clubs) ::(Jack, Clubs) ::(Ten, Clubs) :: Nil

    Hand(cards) should have(
      'ranking(Straight),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand three of a kind") {
    val cards = (Nine, Diamonds) ::(Three, Spades) ::(Three, Hearths) ::
      (Queen, Hearths) ::(Ace, Diamonds) ::(Three, Clubs) ::(Five, Clubs) :: Nil

    val matched = (Three, Spades) ::(Three, Hearths) ::(Three, Clubs) :: Nil
    val kickers = (Ace, Diamonds) ::(Queen, Hearths) :: Nil
    val five = matched ++ kickers

    Hand(cards) should have(
      'ranking(ThreeOfAKind),
      'cards(five),
      'matched(matched)
    )
  }
  it("should understand two pair") {
    val cards = (Jack, Hearths) ::(Eight, Hearths) ::(Three, Spades) ::
      (Three, Hearths) ::(Five, Spades) ::(Two, Clubs) ::(Two, Diamonds) :: Nil

    val matched = (Three, Spades) ::(Three, Hearths) ::(Two, Clubs) ::(Two, Diamonds) :: Nil
    val kicker = (Jack, Hearths)
    val five = matched :+ kicker

    Hand(cards) should have(
      'ranking(TwoPair),
      'cards(five),
      'matched(matched)
    )
  }
  it("should understand one pair") {
    val cards = (Three, Spades) ::(Four, Spades) ::(Five, Spades) ::
      (Six, Clubs) ::(Six, Diamonds) ::(Nine, Diamonds) ::(Ten, Diamonds) :: Nil

    val matched = (Six, Clubs) ::(Six, Diamonds) :: Nil
    val kickers = (Ten, Diamonds) ::(Nine, Diamonds) ::(Five, Spades) :: Nil
    val five = matched ++ kickers

    Hand(cards) should have(
      'ranking(OnePair),
      'cards(five),
      'matched(matched)
    )
  }
  it("should understand high card") {
    val cards = (Ace, Clubs) ::(Seven, Diamonds) ::(Eight, Diamonds) ::
      (Nine, Diamonds) ::(Ten, Diamonds) ::(King, Spades) ::(Three, Spades) :: Nil

    val matched = Nil
    val kickers = (Ace, Clubs) ::(King, Spades) ::(Ten, Diamonds) ::(Nine, Diamonds) ::(Eight, Diamonds) :: Nil
    val five = kickers

    Hand(cards) should have(
      'ranking(HighCard),
      'cards(five),
      'matched(matched)
    )
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
