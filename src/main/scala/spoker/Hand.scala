package spoker

package object hand {

  import spoker.cards._
  import spoker.cards.Rank._

  object Hand {
    def apply(cards: Cards): Hand = {
      cards.sorted(Ordering[Card].reverse) match {
        case Flush(Straight(Broadway(matched))) => new RoyalFlush(matched)
        case Flush(Straight(matched)) => new StraightFlush(matched)
        case Four(OfAKind(it)) => it
        case Three(t, Pair(p, _)) => new FullHouse(t, p)
        case Flush(matched) => new Flush(matched)
        case Straight(matched) => new Straight(matched)
        case Three(OfAKind(it)) => it
        case Pair(first, Pair(second, kickers)) => new TwoPair(first, second, kickers.head)
        case Pair(OfAKind(it)) => it
        case it => new HighCard(it)
      }
    }
  }

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
      groupsOfTwo(cards) lastOption
      match {
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

  object HandRanking extends Enumeration {
    val HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush =
      Value
  }

  sealed abstract class Hand(var c: Cards, var m: Cards) extends Ordered[Hand] {
    val cards = c.take(5)
    val matched = m.take(5)
    val ranking = HandRanking withName this.getClass.getSimpleName

    def this(cards: Cards) = this(cards, cards)

    def compare(that: Hand): Int = this.ranking.compareTo(that.ranking)
  }

  private class RoyalFlush(cards: Cards) extends Hand(cards)

  private class StraightFlush(cards: Cards) extends Hand(cards)

  private class FourOfAKind(matched: Cards, kickers: Cards) extends Hand(matched ++ kickers, matched)

  private class FullHouse(three: Cards, pair: Cards) extends Hand(three ++ pair)

  private class Flush(cards: Cards) extends Hand(cards)

  private class Straight(cards: Cards) extends Hand(cards)

  private class ThreeOfAKind(matched: Cards, kickers: Cards) extends Hand(matched ++ kickers, matched)

  private class TwoPair(
                         first: Cards,
                         second: Cards,
                         kicker: Card) extends Hand(first ++ second :+ kicker, first ++ second)

  private class OnePair(matched: Cards, kickers: Cards) extends Hand(matched ++ kickers, matched)

  private class HighCard(kickers: Cards) extends Hand(kickers, Nil)

}