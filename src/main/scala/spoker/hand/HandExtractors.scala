package spoker.hand

import spoker._
import spoker.hand.HandSpecializations.{FourOfAKind, ThreeOfAKind, OnePair}

private object HandExtractors {
  object Broadway {
    def unapply(cards: Cards) = {
      cards.head.rank match {
        case Ace => Option(cards)
        case _ => None
      }
    }
  }

  object Straight {
    def unapply(cards: Cards): Option[Cards] =
      cards.take(5) :: cards.take(6).takeRight(5) :: cards.takeRight(5) :: Nil find {
        c => (c.last.rank.id to c.last.rank.id + 4).reverse == c.map(_.rank.id)
      } match {
        case matched: Some[Cards] => Option(matched.get)
        case None => None
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