package spoker

package object game {

  import spoker.cards._
  import spoker.cards.Rank._

  object Game extends StraightAwareness with FlushAwareness with PairAwareness with ThreeOfAKindAwareness {
    def apply(implicit cards:Cards): Game = {
      toTuples(cards.sorted) match {
        case (Seq((Ten,_),(Jack,_),(Queen,_),(King,_),(Ace,_))) if flush => new RoyalFlush
        case (_) if straight && flush => new StraightFlush
        case (_) if threeOfAKind => new ThreeOfAKind
        case (_) if twoPairs => new TwoPair
        case (_) if onePair => new OnePair
        case (_) => new HighCard
      }
    }
  }

  trait StraightAwareness {
    def straight(implicit cards:Cards) = {
      val ranks = cards.map(_.rank)
      ranks.takeRight(ranks.size-1).zip(ranks)
        .forall((t) => t._1.id == 1+t._2.id)
    }
  }

  trait FlushAwareness {
    def flush(implicit cards:Cards) = {
      cards.map(_.suit).forall(_ == cards.head.suit)
    }
  }

  trait PairAwareness extends GroupAwareness {
    val numberOfPairs = numberOfGroupsOf(2)_
    def onePair(implicit cards:Cards) = 1 == numberOfPairs(cards)
    def twoPairs(implicit cards:Cards) = 2 == numberOfPairs(cards)
  }

  trait ThreeOfAKindAwareness extends GroupAwareness {
    def threeOfAKind(implicit cards:Cards) = 1 == numberOfGroupsOf(3)(cards)
  }

  trait GroupAwareness {
    protected def numberOfGroupsOf(x:Int)(cards:Cards) = {
      cards.map(_.rank).groupBy(identity)
        .filter(x == _._2.size).size
    }
  }

  sealed abstract class Game(cards: Cards)

  class RoyalFlush(implicit cards: Cards) extends Game(cards)
  class StraightFlush(implicit cards: Cards) extends Game(cards)
  class ThreeOfAKind(implicit cards: Cards) extends Game(cards)
  class TwoPair(implicit cards: Cards) extends Game(cards)
  class OnePair(implicit cards: Cards) extends Game(cards)
  class HighCard(implicit cards: Cards) extends Game(cards)

}