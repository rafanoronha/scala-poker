package spoker.betting.spec

import spoker.betting._
import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import spoker.table._

class BettingSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  var table: Table = null

  var round: BettingRound = null

  before {
    table = Table(
      players = new Player("p1") :: new Player("p2") :: new Player("p3") :: Nil
    ).newHand
    round = table.currentRound.get
  }

  def player(name: String) = table.currentRound.get.betters.find(
    _.name equals name
  ).get

  def smallBlind = player("p1")

  def bigBlind = player("p2")

  def dealer = player("p3")

  describe("Rounds") {
    it("should end when all players acted and no open bet exist") {
      round = table.place(smallBlind.raise(10)).place(bigBlind.call).place(dealer.fold).currentRound.get
      round.hasEnded should be(true)
    }
    it("should end when a bet is not matched") {
      round = table.place(smallBlind.raise(10)).place(bigBlind.fold).place(dealer.fold).currentRound.get
      round.hasEnded should be(true)
    }
    it("should not end while an open bet exist") {
      round = table.place(smallBlind.raise(10)).place(bigBlind.fold).place(dealer.raise(20)).currentRound.get
      round.hasEnded should be(false)
    }
  }

  describe("Players") {
    it("should be able to limp in") {
      table.place(smallBlind.call).place(bigBlind.check).place(dealer.call)
    }
    it("should be able to fold an initial bet") {
      table.place(smallBlind.fold)
    }
    it("should be able to fold a raised bet") {
      table.place(smallBlind.raise(10)).place(bigBlind.fold)
    }
  }
  describe("Check") {
    it("should not be placed by players other than the big blind") {
      evaluating {
        round.place(smallBlind.check)
      } should produce[NonBigBlindCheckException]
    }
    it("should not be placed if any betting action got place") {
      round = table.place(smallBlind.raise(10)).currentRound.get
      evaluating {
        round.place(bigBlind.check)
      } should produce[CantCheckException]
    }
  }

}