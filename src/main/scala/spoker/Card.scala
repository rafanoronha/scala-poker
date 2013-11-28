package spoker

class Card(val rank: Rank.Value, val suit: Suit.Value) extends Ordered[Card] {
  def compare(that: Card): Int = this.rank compare that.rank

  override def equals(that: Any): Boolean = that match {
    case c: Card => toTuple(this) equals toTuple(c)
    case t: CardTuple => toTuple(this) equals t
    case _ => false
  }

  override def hashCode(): Int = toTuple(this).##

  override def toString = toTuple(this).toString
}
