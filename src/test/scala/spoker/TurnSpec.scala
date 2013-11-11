package spoker.betting.spec

import spoker.betting._
import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import spoker.table._

class TurnSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  val smallBlind = new PositionedPlayer(new Player("p1"), SmallBlind)
  val bigBlind = new PositionedPlayer(new Player("p2"), BigBlind)
  val dealer = new PositionedPlayer(new Player("p3"))
  val players = smallBlind :: bigBlind :: dealer :: Nil

  var round: BettingRound = null

  before {
    round = BettingRound(players)
  }

  it("should allow player in turn to place action") {
    round.place(smallBlind.call)
  }
  it("should not allow player out of turn to place action") {
    evaluating {
      round.place(bigBlind.call)
    } should produce[OutOfTurnException]
  }
  it("should advance current turn after an action") {
    round.place(smallBlind.call).place(bigBlind.call)
  }
}