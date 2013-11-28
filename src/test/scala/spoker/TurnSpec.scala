package spoker.betting.spec

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers

import spoker._
import spoker.betting._

class TurnSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

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

  it("should allow player in turn to place action") {
    round.place(smallBlind.call)
  }
  it("should not allow player out of turn to place action") {
    evaluating {
      round.place(bigBlind.call)
    } should produce[OutOfTurnException]
  }
  it("should advance current turn after an action") {
    table.place(smallBlind.call).place(bigBlind.call)
  }
}