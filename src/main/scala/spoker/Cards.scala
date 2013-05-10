package spoker

sealed trait Rank

object Ace extends Rank
object King extends Rank
object Queen extends Rank
object Jack extends Rank
object Ten extends Rank

sealed trait Suit

object Clubs extends Suit
object Diamonds extends Suit
object Hearths extends Suit
object Spades extends Suit