package spoker.table.spec

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import spoker.table._
import spoker.betting.RoundKind._

class TableSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  var table: Table = null

  before {
    table = Table(new Player :: new Player :: new Player :: Nil).newHand
  }

  def playNextRound = {
    val player1 = table betters (0)
    val player2 = table betters (1)
    val player3 = table betters (2)
    table = table.place(player1.call).place(player2.check).place(player3.call)
    table = table.currentRound.get.kind match {
      case River => table
      case _ => table.nextRound
    }
    this
  }

  describe("Table") {
    it("should run a hand through pre-flop to showdown") {
      def currentRoundKind = table.currentRound.get.kind
      currentRoundKind should be(PreFlop)
      playNextRound
      currentRoundKind should be(Flop)
      playNextRound
      currentRoundKind should be(Turn)
      playNextRound
      currentRoundKind should be(River)
      playNextRound
      table.showdown
    }
    it("should not open next round before closing current one") {
      evaluating {
        table.nextRound
      } should produce[UnclosedRoundException]
    }
    it("should not have any betting round after river") {
      playNextRound.playNextRound.playNextRound.playNextRound
      evaluating {
        table.nextRound
      } should produce[NoMoreRoundsException]
    }
    it("should not open next hand before closing current one")(pending)
  }

  describe("Showdown") {
    it("should only take place after the river")(pending)
  }
  describe("Pot") {
    it("should be won by best showdown rank owner")(pending)
    it("should be won by unmatched bet owner")(pending)
    it("should collect the blinds")(pending)
    it("should collect all played stakes")(pending)
    it("should grant all collected value to the hand's winner")(pending)
  }
}