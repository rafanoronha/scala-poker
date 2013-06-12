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

  describe("Rounds") {
    it("should end when all players acted and no open bet exist") {
      round = round.place(smallBlind.raise(10)).place(bigBlind.fold).place(dealer.call)
      round.hasEnded should be(true)
    }
    it("should end when a bet is not matched") {
      round = round.place(smallBlind.raise(10)).place(bigBlind.fold).place(dealer.fold)
      round.hasEnded should be(true)
    }
    it("should not end while an open bet exist") {
      round = round.place(smallBlind.raise(10)).place(bigBlind.fold).place(dealer.raise(20))
      round.hasEnded should be(false)
    }
  }
  describe("Players") {
    it("should be able to limp in") {
      round.place(smallBlind.call).place(bigBlind.check).place(dealer.call)
    }
    it("should be able to fold an initial bet") {
      round.place(smallBlind.fold)
    }
    it("should be able to fold a raised bet") {
      round.place(smallBlind.raise(10)).place(bigBlind.fold)
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