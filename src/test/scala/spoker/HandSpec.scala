package spoker.hand.spec

import spoker._
import spoker.hand.Hand
import spoker.hand.HandRanking._
import org.scalatest.FunSpec
import org.scalatest.Matchers

class HandSpec extends FunSpec with Matchers {

  it("should understand royal flush") {
    val cards = (Nine, Clubs) ::(Ace, Hearts) ::(King, Clubs) ::
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
    val cards = (Four, Spades) ::(Ace, Spades) ::(Ace, Hearts) ::
      (Two, Spades) ::(Ace, Clubs) ::(Ace, Diamonds) ::(Three, Clubs) :: Nil

    val matched = (Ace, Spades) ::(Ace, Hearts) ::(Ace, Clubs) ::(Ace, Diamonds) :: Nil
    val kicker = (Four, Spades)
    val five = matched :+ kicker

    Hand(cards) should have(
      'ranking(FourOfAKind),
      'cards(five),
      'matched(matched)
    )
  }
  it("should understand full house") {
    val cards = (Three, Spades) ::(Nine, Clubs) ::(Ace, Hearts) ::
      (Three, Hearts) ::(Two, Spades) ::(Two, Clubs) ::(Two, Diamonds) :: Nil

    val five = (Two, Spades) ::(Two, Clubs) ::(Two, Diamonds) ::(Three, Spades) ::(Three, Hearts) :: Nil

    Hand(cards) should have(
      'ranking(FullHouse),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand flush") {
    val cards = (Seven, Hearts) ::(Queen, Hearts) ::(Ace, Spades) ::
      (Ace, Hearts) ::(Nine, Hearts) ::(Two, Hearts) ::(Five, Hearts) :: Nil

    val five = (Ace, Hearts) ::(Queen, Hearts) ::(Nine, Hearts) ::(Seven, Hearts) ::(Five, Hearts) :: Nil

    Hand(cards) should have(
      'ranking(Flush),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand broadway straight") {
    val cards = (Ace, Spades) ::(King, Clubs) ::(Four, Hearts) ::
      (Four, Spades) ::(Queen, Clubs) ::(Jack, Clubs) ::(Ten, Clubs) :: Nil

    val five = (Ace, Spades) ::(King, Clubs) ::(Queen, Clubs) ::(Jack, Clubs) ::(Ten, Clubs) :: Nil

    Hand(cards) should have(
      'ranking(Straight),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand wheel straight") {
    val cards = (Five, Clubs) ::(Two, Clubs) ::(Jack, Clubs) ::(Ten, Clubs) ::(Three, Hearts) ::(Ace, Hearts) ::(Four, Spades) :: Nil

    val five = (Five, Clubs) ::(Four, Spades) ::(Three, Hearts) ::(Two, Clubs) ::(Ace, Hearts) :: Nil

    Hand(cards) should have(
      'ranking(Straight),
      'cards(five),
      'matched(five)
    )
  }
  it("should understand that wheel straight flush is not a royal flush") {
    val cards = (Five, Clubs) ::(Two, Clubs) ::(Jack, Hearts) ::(Ten, Hearts) ::(Three, Clubs) ::(Ace, Clubs) ::(Four, Clubs) :: Nil

    val five = (Five, Clubs) ::(Four, Clubs) ::(Three, Clubs) ::(Two, Clubs) ::(Ace, Clubs) :: Nil

    Hand(cards) should have(
      'ranking(StraightFlush),
      'cards(five),
      'matched(five)
    )
    
    Hand(cards) should not have(
        'ranking(RoyalFlush)
    )
  }
  it("should understand straight when there is a pair of cards in the middle of straight") {
    val cards = (Ten, Spades) ::(Nine, Clubs) ::(Nine, Hearts) ::
      (Four, Spades) ::(Eight, Clubs) ::(Jack, Clubs) ::(Seven, Clubs) :: Nil

    Hand(cards) should have(
      'ranking(Straight)
    )
  }
  it("should understand royal flush when there is a pair of cards") {
    val cards = (Ace, Clubs) ::(King, Clubs) ::(Ten, Clubs) ::(Queen, Hearts) ::
      (Four, Spades) ::(Queen, Clubs) ::(Jack, Clubs) :: Nil

    Hand(cards) should have(
      'ranking(RoyalFlush)
    )
  }
  
  it("should understand three of a kind") {
    val cards = (Nine, Diamonds) ::(Three, Spades) ::(Three, Hearts) ::
      (Queen, Hearts) ::(Ace, Diamonds) ::(Three, Clubs) ::(Five, Clubs) :: Nil

    val matched = (Three, Spades) ::(Three, Hearts) ::(Three, Clubs) :: Nil
    val kickers = (Ace, Diamonds) ::(Queen, Hearts) :: Nil
    val five = matched ++ kickers

    Hand(cards) should have(
      'ranking(ThreeOfAKind),
      'cards(five),
      'matched(matched)
    )
  }
  it("should understand two pair") {
    val cards = (Jack, Hearts) ::(Eight, Hearts) ::(Three, Spades) ::
      (Three, Hearts) ::(Five, Spades) ::(Two, Clubs) ::(Two, Diamonds) :: Nil

    val matched = (Three, Spades) ::(Three, Hearts) ::(Two, Clubs) ::(Two, Diamonds) :: Nil
    val kicker = (Jack, Hearts)
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
      Hand(Seq((Three, Spades), (Three, Hearts), (Two, Spades), (Two, Clubs), (Two, Diamonds))),
      Hand(Seq((Three, Spades), (Three, Hearts), (Five, Spades), (Two, Clubs), (Two, Diamonds))))
      .map(_.ranking)
    actual should equal(Seq(RoyalFlush, FullHouse, TwoPair))
  }
  it("should compare different hands by ranking") {
    val (royalFlush, straightFlush, straight, fullHouse, twoPair, onePair) =
      (Hand(Seq((Ace, Clubs), (King, Clubs), (Queen, Clubs), (Jack, Clubs), (Ten, Clubs))),
        Hand(Seq((Six, Diamonds), (Seven, Diamonds), (Eight, Diamonds), (Nine, Diamonds), (Ten, Diamonds))),
        Hand(Seq((Ace, Spades), (King, Clubs), (Queen, Clubs), (Jack, Clubs), (Ten, Clubs))),
        Hand(Seq((Three, Spades), (Three, Hearts), (Two, Spades), (Two, Clubs), (Two, Diamonds))),
        Hand(Seq((Three, Spades), (Three, Hearts), (Five, Spades), (Two, Clubs), (Two, Diamonds))),
        Hand(Seq((Three, Spades), (Four, Spades), (Five, Spades), (Six, Clubs), (Six, Diamonds))))

    royalFlush should be > straightFlush
    straight should be < fullHouse
    twoPair should be > onePair
  }
  it("should rank royal flush as higher than straight flush") {
    RoyalFlush should be > StraightFlush
  }
  it("should rank straight flush as higher than four of a kind") {
    StraightFlush should be > FourOfAKind
  }
  it("should rank four of a kind as higher than full house") {
    FourOfAKind should be > FullHouse
  }
  it("should rank full house as higher than flush") {
    FullHouse should be > Flush
  }
  it("should rank flush as higher than straight") {
    Flush should be > Straight
  }
  it("should rank straight as higher than three of a kind") {
    Straight should be > ThreeOfAKind
  }
  it("should rank three of a kind as higher than two pair") {
    ThreeOfAKind should be > TwoPair
  }
  it("should rank two pair as higher than one pair") {
    TwoPair should be > OnePair
  }
  it("should rank one pair as higher than high card") {
    OnePair should be > HighCard
  }
  it("should rank high card as less than full house") {
    HighCard should be < FullHouse
  }
  it("should rank high card Ace as higher than high card King") {
    val handAceHigh = Hand(Seq((Ace, Clubs), (Seven, Diamonds), (Eight, Diamonds), 
      (Nine, Diamonds), (Ten, Diamonds), (King, Spades), (Three, Spades)))
    val handKingHigh = Hand(Seq((Five, Clubs), (Seven, Diamonds), (Eight, Diamonds), 
      (Nine, Diamonds), (Ten, Diamonds), (King, Spades), (Three, Spades)))
    handAceHigh should be > handKingHigh
  }
  it("should use kicker when top cards have the same rank") {
    val handSevenKicker = Hand(Seq((Ace, Clubs), (Seven, Diamonds), (Two, Diamonds), 
      (Nine, Diamonds), (Ten, Diamonds), (King, Spades), (Three, Spades)))
    val handEightKicker = Hand(Seq((Ace, Hearts), (Eight, Clubs), (Two, Spades), 
      (Nine, Hearts), (Ten, Spades), (King, Diamonds), (Three, Hearts)))
    handEightKicker should be > handSevenKicker
  }
  it("should rank six pair higher than five pair") {
    val handSixPair = Hand(Seq((Three, Spades), (Four, Spades), (Jack, Spades), (Six, Clubs), (Six, Diamonds), (Ten, Hearts), (King, Hearts)))
    val handFivePair = Hand(Seq((Three, Diamonds), (Four, Clubs), (Jack, Clubs), (Five, Hearts), (Five, Clubs), (Ten, Spades), (Ace, Spades)))
    handSixPair should be > handFivePair
  }
  it("should use kicker than single pairs have the same rank") {
    val handKingKicker = Hand(Seq((Three, Spades), (Four, Spades), (Five, Spades), (Six, Clubs), (Six, Diamonds), (Ten, Hearts), (King, Hearts)))
    val handAceKicker = Hand(Seq((Three, Diamonds), (Four, Clubs), (Five, Clubs), (Six, Hearts), (Six, Clubs), (Ten, Spades), (Ace, Spades)))
    handAceKicker should be > handKingKicker
  }
  it("should use kicker than two pairs have the same rank") {
    val handKingKicker = Hand(Seq((Three, Spades), (Three, Hearts), (Five, Spades), (Six, Clubs), (Six, Diamonds), (Ten, Hearts), (King, Hearts)))
    val handJackKicker = Hand(Seq((Three, Diamonds), (Three, Clubs), (Five, Clubs), (Six, Hearts), (Six, Clubs), (Ten, Spades), (Jack, Spades)))
    handKingKicker should be > handJackKicker
  }
  it("should compare top pairs first") {
    val handFirstPairJacks = Hand(Seq((Three, Spades), (Three, Hearts), (Five, Spades), (Jack, Clubs), (Jack, Diamonds), (Ten, Hearts), (King, Hearts)))
    val handFirstPairNines = Hand(Seq((Seven, Diamonds), (Seven, Clubs), (Five, Clubs), (Nine, Hearts), (Nine, Clubs), (Ten, Spades), (Jack, Spades)))
    handFirstPairJacks should be > handFirstPairNines
  }
  it("should compare second pairs if top pairs are equal") {
    val handSecondPairThrees = Hand(Seq((Three, Spades), (Three, Hearts), (Five, Spades), (Jack, Clubs), (Jack, Diamonds), (Ten, Hearts), (King, Hearts)))
    val handSecondPairSevens = Hand(Seq((Seven, Diamonds), (Seven, Clubs), (Five, Clubs), (Jack, Hearts), (Jack, Clubs), (Ten, Spades), (Jack, Spades)))
    handSecondPairThrees should be < handSecondPairSevens
  }
  it("should rank three of nines higher than three of fives") {
    val handThreeOfFives = Hand(Seq((Three, Spades), (Four, Spades), (Five, Spades), (Five, Clubs), (Five, Diamonds), (Ten, Hearts), (King, Hearts)))
    val handThreeOfNines = Hand(Seq((Three, Spades), (Four, Spades), (Nine, Spades), (Nine, Clubs), (Nine, Diamonds), (Ten, Hearts), (King, Hearts)))
    handThreeOfNines should be > handThreeOfFives
  }
  it("should rank queen straight higher than jack straight") {
    val queenStraight = Hand(Seq((Queen, Clubs), (Jack, Clubs), (Ten, Clubs),(Nine, Spades), (Eight, Clubs), (Three, Hearts), (Five, Spades)))    
    val jackStraight = Hand(Seq((Jack, Clubs), (Ten, Clubs),(Nine, Spades), (Eight, Clubs),(Seven, Clubs), (Three, Hearts), (Five, Spades)))
    queenStraight should be > jackStraight
  }
  it("should use a kicker if top cards of the straight have the same rank") {
    val queenStraightNineKicker = Hand(Seq((Queen, Clubs), (Jack, Clubs), (Ten, Clubs),(Nine, Spades), (Eight, Clubs), (Three, Hearts), (Five, Spades)))    
    val queenStraightEightKicker = Hand(Seq((Queen, Spades), (Jack, Spades), (Ten, Spades),(Eight, Spades), (Eight, Diamonds), (Three, Diamonds), (Five, Clubs)))    
    queenStraightNineKicker should be > queenStraightEightKicker
  }
  it("should rank jack flush higher than ten flush ") {
    val jackFlush = Hand(Seq((Six, Diamonds), (Seven, Diamonds), (Eight, Diamonds), (Nine, Diamonds), (Jack, Diamonds), (Three, Hearts), (Five, Spades)))
    val tenFlush = Hand(Seq((Five, Clubs), (Seven, Clubs), (Eight, Clubs), (Nine, Clubs), (Ten, Clubs), (Three, Diamonds), (Five, Hearts)))
    jackFlush should be > tenFlush
  }
  it("should use a kicker if top cards of the flush have the same rank") {
    val jackFlushSixKicker = Hand(Seq((Six, Diamonds), (Seven, Diamonds), (Eight, Diamonds), (Nine, Diamonds), (Jack, Diamonds), (Three, Hearts), (Five, Spades)))
    val jackFlushFiveKicker = Hand(Seq((Five, Clubs), (Seven, Clubs), (Eight, Clubs), (Nine, Clubs), (Jack, Clubs), (Three, Diamonds), (Five, Hearts)))
    jackFlushSixKicker should be > jackFlushFiveKicker
  }
  it("should rank full house with three kings higher than full house with three jacks") {
    val threeKingsTwoFours = Hand(Seq((King, Diamonds), (King, Clubs), (Four, Diamonds), (Jack, Diamonds), (Four, Hearts), (King, Hearts), (Queen, Spades)))
    val threeJacksTwoAces = Hand(Seq((Jack, Spades), (Jack, Hearts), (Jack, Clubs), (Ace, Diamonds), (Five, Clubs), (Ace, Hearts), (Queen, Hearts)))
    threeKingsTwoFours should be > threeJacksTwoAces
  }
  it("should rank four sevens higher than four fives") {
    val fourSevens = Hand(Seq((Seven, Diamonds), (Seven, Clubs), (Seven, Hearts), (Seven, Diamonds), (Eight, Clubs), (Three, Hearts), (Five, Spades)))
    val fourFives = Hand(Seq((Nine, Diamonds), (Ten, Hearts), (King, Hearts), (Five, Diamonds), (Five, Clubs), (Five, Hearts), (Five, Diamonds)))
    fourSevens should be > fourFives
  }
  it("should rank jack straight flush higher than ten straight flush ") {
    val jackStraightFlush = Hand(Seq((Ten, Diamonds), (Seven, Diamonds), (Eight, Diamonds), (Nine, Diamonds), (Jack, Diamonds), (Three, Hearts), (Five, Spades)))
    val tenStraightFlush = Hand(Seq((Six, Clubs), (Seven, Clubs), (Eight, Clubs), (Nine, Clubs), (Ten, Clubs), (Three, Diamonds), (Five, Hearts)))
    jackStraightFlush should be > tenStraightFlush
  }
  it("should rank two royal flushes equally") {
    val diamondRoyalFlush = Hand(Seq((Ten, Diamonds), (Ace, Diamonds), (Queen, Diamonds), (King, Diamonds), (Jack, Diamonds), (Three, Hearts), (Five, Diamonds)))
    val clubRoyalFlush = Hand(Seq((Ten, Clubs), (Ace, Clubs), (Queen, Clubs), (King, Clubs), (Jack, Clubs), (Three, Hearts), (Five, Clubs)))
    diamondRoyalFlush should equal(clubRoyalFlush)
  }
  it("should rank two equal hands equally") {
    val hand1 = Hand(Seq((Three, Spades), (Four, Spades), (Jack, Spades), (Six, Clubs), (Six, Diamonds), (Ten, Hearts), (King, Hearts)))
    val hand2 = Hand(Seq((Jack, Diamonds), (Six, Hearts), (Three, Diamonds), (Four, Diamonds), (Six, Spades), (Ten, Clubs), (King, Clubs)))

    hand1 should equal(hand2)
  }

}
