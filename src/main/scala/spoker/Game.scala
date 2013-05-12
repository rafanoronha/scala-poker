package spoker

package object game {

  import spoker.cards._
  import spoker.cards.Rank._

  object Game extends StraightAwareness with FlushAwareness {
    def apply(implicit cards:Cards): Game = {
      toTuples(cards.sorted) match {
        case (Seq((Ten,_),(Jack,_),(Queen,_),(King,_),(Ace,_))) if flush => new RoyalFlush
        case (_) if straight && flush => new StraightFlush
      }
    }
  }

  trait StraightAwareness {
    def straight(implicit cards:Cards) = {
      val ranks = cards.map(_.rank)
      5 == ranks.toSet.size && ranks.head.id == 4 - ranks.last.id
    }
  }

  trait FlushAwareness {
    def flush(implicit cards:Cards) = 1 == cards.map(_.suit).toSet.size
  }

  sealed abstract class Game(cards: Cards)

  class RoyalFlush(implicit cards: Cards) extends Game(cards)
  class StraightFlush(implicit cards: Cards) extends Game(cards)
}