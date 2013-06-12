package spoker.betting.spec

import spoker.betting._
import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers

class TurnSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  val smallBlind = new Player
  val bigBlind = new BigBlindPlayer
  val dealer = new Player

  var round: BettingRound = null

  before {
    round = BettingRound(smallBlind :: bigBlind :: dealer :: Nil)
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