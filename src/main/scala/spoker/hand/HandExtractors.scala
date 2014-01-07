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
      val longestStraight: Tuple2[Int, Cards] = cards.foldLeft(Tuple2(0, List.empty[Card]))((resultPair, card) => {
        if (resultPair._1 == 5 || (resultPair._1 > 0 && card.rank == resultPair._2.head.rank)) //already found the straight or have a pair
          resultPair
        else if (resultPair._1 == 0 || card.rank.id < (resultPair._2.head.rank.id - 1)) //restart straight counting
          (1, card :: Nil)
        else
          (resultPair._1 + 1, card :: resultPair._2)
      })
      
      if (longestStraight._1 >= 5)
        Option(longestStraight._2.reverse)
      else if (longestStraight._1 == 4 && longestStraight._2.head.rank == Two && cards.head.rank == Ace) //wheel straight
        Option(longestStraight._2.reverse :+ cards.head)
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