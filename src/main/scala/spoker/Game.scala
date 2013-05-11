package spoker

import Card._
import Rank._

object Game {

  def apply(cards: Seq[Card]): Game = {

    val sameSuit = 1 == cards.map(_.suit).toSet.size

    toTuples(cards.sorted) match {
      case (Seq((Ten,_),(Jack,_),(Queen,_),(King,_),(Ace,_))) if sameSuit => new RoyalFlush(cards)
    }
  }
}

sealed abstract class Game(cards: Seq[Card])

class RoyalFlush(cards: Seq[Card]) extends Game(cards)