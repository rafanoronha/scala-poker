package spoker.betting.spec

import org.scalatest.{ BeforeAndAfter, FunSpec }
import org.scalatest.Matchers
import spoker._
import spoker.betting._
import org.scalatest.Tag

class TurnSpec extends FunSpec with Matchers with BeforeAndAfter {

  var table: Table = null

  var round: BettingRound = null

  before {
    table = Table(
      players = new PositionedPlayer(new Player("p1"), initialStack = 1500) ::
        new PositionedPlayer(new Player("p2"), initialStack = 1500) ::
        new PositionedPlayer(new Player("p3"), initialStack = 1500, isButton = true) :: Nil).newHand
    round = table.currentRound.get
  }

  def player(name: String) : Better = table.currentRound.get.players.find(
    _.positionedPlayer.name equals name).get

  def smallBlind = player("p1")

  def bigBlind = player("p2")

  def dealer = player("p3")

  it("should allow player in turn to place action") {
    round.place(dealer.call)
  }
  it("should not allow player out of turn to place action") {
    evaluating {
      round.place(smallBlind.call)
    } should produce[OutOfTurnException]
  }
  it("should advance current turn after an action") {
    table.place(dealer.call).place(smallBlind.call)
  }
  it("should correctly advance current turn after every given action") {
    table = Table(
      players = new PositionedPlayer(new Player("p1"), initialStack = 1500) ::
        new PositionedPlayer(new Player("p2"), initialStack = 1500, isButton = true) ::
        new PositionedPlayer(new Player("p3"), initialStack = 1500) ::
        new PositionedPlayer(new Player("p4"), initialStack = 1500) ::
        new PositionedPlayer(new Player("p5"), initialStack = 1500) ::
        new PositionedPlayer(new Player("p6"), initialStack = 1500) ::
        new PositionedPlayer(new Player("p7"), initialStack = 1500) ::
        new PositionedPlayer(new Player("p8"), initialStack = 1500) :: Nil).newHand
    table
      .place(player("p5").raise(4))
      .place(player("p6").fold)
      .place(player("p7").call)
      .place(player("p8").call)
      .place(player("p1").raise(6))
      .place(player("p2").raise(8))
      .place(player("p3").call)
      .place(player("p4").call)
      .place(player("p5").call)
      .place(player("p7").fold)
      .place(player("p8").call)
      .place(player("p1").call).nextRound
      .place(player("p3").bet(2))
      .place(player("p4").fold)
      .place(player("p5").fold)
      .place(player("p8").call)
      .place(player("p1").fold)
      .place(player("p2").raise(4))
      .place(player("p3").raise(6))
      .place(player("p8").call)
      .place(player("p2").fold).nextRound
      .place(player("p3").bet(2))
      .place(player("p8").fold)
  }
  
  describe("hasEnded") {
    it("should return false if players haven't made any moves yet") {
      round.hasEnded should equal(false)
    }
    it("should return false after limping by small blind, because big blind ends preflop round") {
      table = table.place(dealer.call).place(smallBlind.call)
      table.currentRound.get.hasEnded should equal(false)
    }
    it("should return true when big blind ends pre-flop round") {
      table = table.place(dealer.call).place(smallBlind.call).place(bigBlind.check)
      table.currentRound.get.hasEnded should equal(true)
    }
    it("should return false if the round wasn't open yet") {
      table = table.place(dealer.call).place(smallBlind.call).place(bigBlind.check).nextRound.place(smallBlind.check)
      table.currentRound.get.hasEnded should equal(false)
    }
  }
}