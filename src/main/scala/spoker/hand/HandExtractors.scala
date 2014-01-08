package spoker.hand

import spoker._
import spoker.hand.HandSpecializations.{FourOfAKind, ThreeOfAKind, OnePair}

private object HandExtractors {
  object Broadway {
    def unapply(cards: Cards) = {
      cards.take(5).map(_.rank) match {
        case List(Ace, King, Queen, Jack, Ten) => Option(cards)
        case _ => None
      }
    }
  }

  object Straight {
    def unapply(cards: Cards): Option[Cards] = {
      val longestStraight: Cards = cards.tail.foldLeft(List(cards.head))((result, card) => {
        if (result.size == 5 || (result.size > 0 && card.rank == result.head.rank)) //already found the straight or have a pair
          result
        else if (result.size == 0 || card.rank.id < (result.head.rank.id - 1)) //restart straight counting
          card :: Nil
        else
          card :: result
      })
      
      if (longestStraight.size == 5)
        Option(longestStraight.reverse)
      else if (longestStraight.size == 4 && longestStraight.head.rank == Two && cards.head.rank == Ace) //wheel straight
        Option(longestStraight.reverse :+ cards.head)
      else
        None
    }
  }

  object Flush {
    def unapply(cards: Cards) = {
      val upper = (cards groupBy {
        _.suit
      } maxBy {
        _._2.size
      })._1

      cards filter {
        _.suit == upper
      } match {
        case it if it.size > 4 => Option(it)
        case _ => None
      }
    }
  }

  object MatchedAndRest {
    def apply(cards: Cards, matched: Cards) = Option(matched, cards diff matched)
  }

  object Pair {
    def unapply(cards: Cards) =
      groupsOfTwo(cards) lastOption match {
        case None => None
        case Some(matched) => MatchedAndRest(cards, matched)
      }
  }

  object Three {
    def unapply(cards: Cards) =
      groupOf(3)(cards) match {
        case None => None
        case Some(matched) => MatchedAndRest(cards, matched)
      }
  }

  object Four {
    def unapply(cards: Cards) =
      groupOf(4)(cards) match {
        case None => None
        case Some(matched) => MatchedAndRest(cards, matched)
      }
  }

  object OfAKind {
    def unapply(t: (Cards, Cards)): Option[Hand] = {
      val (matched, rest) = t
      val kickers = rest.take(5 - matched.size)
      matched.size match {
        case 4 => Option(new FourOfAKind(matched, kickers))
        case 3 => Option(new ThreeOfAKind(matched, kickers))
        case 2 => Option(new OnePair(matched, kickers))
      }
    }
  }

  private def groupOf(size: Int)(cards: Cards) =
    cards groupBy {
      _.rank
    } map {
      _._2
    } find {
      size == _.size
    }

  private def groupsOfTwo(cards: Cards) =
    (cards groupBy {
      _.rank
    } map {
      _._2
    } filter {
      2 == _.size
    }).toList sortBy {
      _.head.rank
    }
}