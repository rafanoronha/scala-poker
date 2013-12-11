package spoker.dealer

import spoker._

trait CardsDealing {
  def nextCardTo(target: DealtCardTarget): Card
}

object CardsDealing extends CardsDealing {
  def nextCardTo(target: DealtCardTarget): Card = (Ace, Clubs)
}