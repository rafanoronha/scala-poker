package spoker.hand

import spoker.{Card, Cards}
import spoker.hand.HandExtractors._
import spoker.hand.{HandSpecializations => S}

object Hand{
  def apply(cards: Cards): Hand = {
    cards.sorted(Ordering[Card].reverse) match {
      case Flush(Straight(Broadway(matched))) => new S.RoyalFlush(matched)
      case Flush(Straight(matched)) => new S.StraightFlush(matched)
      case Four(OfAKind(it)) => it
      case Three(t, Pair(p, _)) => new S.FullHouse(t, p)
      case Flush(matched) => new S.Flush(matched)
      case Straight(matched) => new S.Straight(matched)
      case Three(OfAKind(it)) => it
      case Pair(first, Pair(second, kickers)) => new S.TwoPair(first, second, kickers.head)
      case Pair(OfAKind(it)) => it
      case it => new S.HighCard(it)
    }
  }
}

sealed abstract class Hand(var c: Cards, var m: Cards) extends Ordered[Hand] {
  val cards = c.take(5)
  val matched = m.take(5)
  val ranking = HandRanking withName this.getClass.getSimpleName

  def this(cards: Cards) = this(cards, cards)

  def compare(that: Hand): Int = {
    val rankCompare = this.ranking.compareTo(that.ranking)
    if (rankCompare != 0)
      rankCompare
    else {
      (cards zip that.cards).map((pair) => pair._1 compare pair._2).find(_ != 0) match {
        case None => 0
        case Some(compareResult) => compareResult
      }
    }
  }
  
  override def equals(that : Any) : Boolean = {
    that match {
      case x: Hand => (compare(x) == 0)
      case _ => (that == this)
    }
  }
  
  override def toString(): String = {
    "rank: %s, cards: %s, matched: %s".format(ranking, cards.mkString, matched.mkString)
  }
}

private object HandSpecializations {
  class RoyalFlush(cards: Cards) extends Hand(cards)

  class StraightFlush(cards: Cards) extends Hand(cards)

  class FourOfAKind(matched: Cards, kickers: Cards) extends Hand(matched ++ kickers, matched)

  class FullHouse(three: Cards, pair: Cards) extends Hand(three ++ pair)

  class Flush(cards: Cards) extends Hand(cards)

  class Straight(cards: Cards) extends Hand(cards)

  class ThreeOfAKind(matched: Cards, kickers: Cards) extends Hand(matched ++ kickers, matched)

  class TwoPair(
    first: Cards,
    second: Cards,
    kicker: Card) extends Hand(first ++ second :+ kicker, first ++ second)

  class OnePair(matched: Cards, kickers: Cards) extends Hand(matched ++ kickers, matched)

  class HighCard(kickers: Cards) extends Hand(kickers, Nil)
}