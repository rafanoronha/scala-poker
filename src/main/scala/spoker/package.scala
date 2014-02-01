package object spoker {

  type Cards = Seq[Card]
  type CardTuple = (Rank.Value, Suit.Value)
  type CardTuples = Seq[CardTuple]

  object Rank extends Enumeration {
    val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
  }

  object Suit extends Enumeration {
    val Clubs, Diamonds, Hearts, Spades = Value
  }
  
  object PlayerStatus {
    trait Status
    
    case object Active extends Status
    case object Inactive extends Status
    case object Folded extends Status
    case object AllIn extends Status
  }

  case class Player(name: String)

  implicit def fromTuple(tuple: CardTuple) = new Card(tuple._1, tuple._2)

  implicit def fromTuples(tuples: CardTuples) = tuples map fromTuple _

  implicit def toTuple(card: Card) = (card.rank, card.suit)

  implicit def toTuples(cards: Cards) = cards map toTuple _

  val Two = Rank.Two
  val Three = Rank.Three
  val Four = Rank.Four
  val Five = Rank.Five
  val Six = Rank.Six
  val Seven = Rank.Seven
  val Eight = Rank.Eight
  val Nine = Rank.Nine
  val Ten = Rank.Ten
  val Jack = Rank.Jack
  val Queen = Rank.Queen
  val King = Rank.King
  val Ace = Rank.Ace

  val Clubs = Suit.Clubs
  val Diamonds = Suit.Diamonds
  val Hearts = Suit.Hearts
  val Spades = Suit.Spades
}