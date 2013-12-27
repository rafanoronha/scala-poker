package spoker.dealer

import spoker.betting.PositionedPlayer
import spoker.Player

trait Dealer {
  val cardsDealing: CardsDealing
  val tableName: String
  val players: Seq[PositionedPlayer]

  def dealFlopCards: Unit = for {
    c <- 1 to 3
  } yield CardsManagement.report(tableName, tableName, cardsDealing.nextCardTo(Community))

  def dealHoleCards: Unit = for {
    c <- 1 to 2
    b <- players
  } yield CardsManagement.report((tableName, b.name, cardsDealing.nextCardTo(PlayerReceivingCard(b.name))))

  def dealNextCommunityCard: Unit =
    CardsManagement.report(tableName, tableName, cardsDealing.nextCardTo(Community))
}