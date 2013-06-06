package spoker.betting.spec

import spoker.betting._
import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers

class BettingSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  val smallBlind = new Player
  val bigBlind = new BigBlindPlayer
  val dealer = new Player

  var round: BettingRound = null

  before {
    round = BettingRound(smallBlind :: bigBlind :: dealer :: Nil)
  }

  describe("Players") {

    it("should be able to limp in") {
      round.place(smallBlind.call).place(bigBlind.check).place(dealer.call)
    }
  }
  describe("Check") {
    it("should not be placed by players other than the big blind") {
      evaluating {
        round.place(smallBlind.check)
      } should produce[NonBigBlindCheckException]
    }
    it("should not be placed if any betting action got place") {
      round = round.place(smallBlind.raise(10))
      evaluating {
        round.place(bigBlind.check)
      } should produce[CantCheckException]
    }
  }

}