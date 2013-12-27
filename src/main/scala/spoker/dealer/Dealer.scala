package spoker.dealer

import spoker.Player
import spoker.ManageablePlayer

trait Dealer {
  val cardsDealing: CardsDealing
  val cardsManagement: CardsManagement
  val players: Seq[ManageablePlayer]

  val communityName = Community.getClass().getSimpleName()

  def dealFlopCards: Unit = for {
    c <- 1 to 3
  } yield cardsManagement.report(communityName, cardsDealing.nextCardTo(Community))

  def dealHoleCards: Unit = for {
    c <- 1 to 2
    b <- players
  } yield cardsManagement.report(b.name, cardsDealing.nextCardTo(PlayerReceivingCard(b.name)))

  def dealNextCommunityCard: Unit =
    cardsManagement.report(communityName, cardsDealing.nextCardTo(Community))
}