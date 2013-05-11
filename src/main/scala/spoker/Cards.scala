package spoker

object Rank extends Enumeration {
  val Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
}

sealed trait Suit

object Clubs extends Suit
object Diamonds extends Suit
object Hearths extends Suit
object Spades extends Suit

class Card(val rank: Rank.Value, val suit: Suit) extends Ordered[Card]{
  def compare(that: Card): Int = this.rank compare that.rank
}

object Card {
  implicit def fromTuple (tuple:(Rank.Value, Suit)) = new Card(tuple._1, tuple._2)
  implicit def fromTuples(tuples:Seq[(Rank.Value, Suit)]) = tuples.map(fromTuple _)
  implicit def toTuple(card:Card) = (card.rank, card.suit)
  implicit def toTuples(cards:Seq[Card]):Seq[(Rank.Value, Suit)] = cards.map(toTuple _)
}