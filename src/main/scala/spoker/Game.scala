package spoker

object Game {

  def apply(cards: Seq[(Rank, Suit)]): Game = {

    val sameSuit = 1 == cards.map(_._2).toSet.size

    cards match {
      case (Seq((Ace,_),(King,_),(Queen,_),(Jack,_),(Ten,_))) if sameSuit => new RoyalFlush(cards)
    }
  }
}

sealed abstract class Game(cards: Seq[(Rank, Suit)])

class RoyalFlush(cards: Seq[(Rank, Suit)]) extends Game(cards)