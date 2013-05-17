package spoker

package object cards {

  type Cards = Seq[Card]
  type CardTuple = (Rank.Value, Suit.Value)
  type CardTuples = Seq[CardTuple]

  class Card(val rank: Rank.Value, val suit: Suit.Value) extends Ordered[Card] {
    def compare(that: Card): Int = this.rank compare that.rank

    override def toString = toTuple(this).toString
  }

  object Rank extends Enumeration {
    val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
  }

  object Suit extends Enumeration {
    val Clubs, Diamonds, Hearths, Spades = Value
  }

  implicit def fromTuple(tuple: CardTuple) = new Card(tuple._1, tuple._2)

  implicit def fromTuples(tuples: CardTuples) = tuples.map(fromTuple _)

  implicit def toTuple(card: Card) = (card.rank, card.suit)

  implicit def toTuples(cards: Cards) = cards.map(toTuple _)
}