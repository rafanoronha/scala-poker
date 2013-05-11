package spoker

import Card._
import Rank._

object Game extends StraightAwareness with FlushAwareness {
  def apply(implicit cards:Seq[Card]): Game = {
    toTuples(cards.sorted) match {
      case (Seq((Ten,_),(Jack,_),(Queen,_),(King,_),(Ace,_))) if flush => new RoyalFlush
      case (_) if straight && flush => new StraightFlush
    }
  }
}

trait StraightAwareness {
  def straight(implicit cards:Seq[Card]) = {
    val ranks = cards.map(_.rank)
    5 == ranks.toSet.size && ranks.head.id == 4 - ranks.last.id
  }
}

trait FlushAwareness {
  def flush(implicit cards:Seq[Card]) = 1 == cards.map(_.suit).toSet.size
}

sealed abstract class Game(cards: Seq[Card])

class RoyalFlush(implicit cards: Seq[Card]) extends Game(cards)
class StraightFlush(implicit cards: Seq[Card]) extends Game(cards)