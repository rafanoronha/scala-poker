package spoker.betting.spec

import org.scalatest.{ BeforeAndAfter, FunSpec }
import org.scalatest.matchers.ShouldMatchers

import spoker._
import spoker.betting._

class BettingSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  var table: Table = null

  var round: BettingRound = null

  before {
    table = Table(
      players = new PositionedPlayer(player = new Player("p1")) ::
        new PositionedPlayer(player = new Player("p2")) ::
        new PositionedPlayer(player = new Player("p3"), isButton = true) :: Nil).newHand
    round = table.currentRound.get
  }

  def player(name: String): Better = table.currentRound.get.players.find(
    _.positionedPlayer.name equals name).get

  def smallBlind = player("p1")

  def bigBlind = player("p2")

  def dealer = player("p3")

  describe("Rounds") {
    it("should end when all players acted and no open bet exist") {
      round = table.place(dealer.fold).place(smallBlind.raise(10)).place(bigBlind.call).currentRound.get
      round.hasEnded should be(true)
    }
    it("should end when a bet is not matched") {
      round = table.place(dealer.fold).place(smallBlind.raise(10)).place(bigBlind.fold).currentRound.get
      round.hasEnded should be(true)
    }
    it("should not end while an open bet exist") {
      round = table.place(dealer.raise(10)).place(smallBlind.raise(20)).place(bigBlind.fold).currentRound.get
      round.hasEnded should be(false)
    }
  }

  describe("Players") {
    it("should be able to limp in") {
      table.place(dealer.call).place(smallBlind.call).place(bigBlind.check)
    }
    it("should be able to fold an initial bet") {
      table.place(dealer.fold)
    }
    it("should be able to fold a raised bet") {
      table.place(dealer.raise(10)).place(smallBlind.fold)
    }
  }
  describe("Check") {
    it("should not be placed if any betting action got place") {
      round = table.place(dealer.call).place(smallBlind.raise(10)).place(bigBlind.call).currentRound.get
      evaluating {
        round.place(dealer.check)
      } should produce[CantCheckException]
    }
  }
  describe("Bet") {
    it("should not be placed if other bet is open") {
      round = table.place(dealer.call).place(smallBlind.raise(10)).place(bigBlind.call).currentRound.get
      evaluating {
        round.place(dealer.bet(5))
      } should produce[CantBetException]
    }
  }

}