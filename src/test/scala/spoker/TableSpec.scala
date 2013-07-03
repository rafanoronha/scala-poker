package spoker.table.spec

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import spoker.table._

class TableSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  var table: Table = null

  before {
    table = Table(new Player :: new Player :: new Player :: Nil).newHand
  }

  describe("Table") {
    it("should run a hand through pre-flop to showdown") {
      playNextRound.playNextRound.playNextRound.playNextRound
      table.showdown
    }
    it("should not open next round before closing current one") {
      table = table.nextRound
      evaluating {
        table.nextRound
      } should produce[UnclosedRoundException]
    }
    it("should handle the river as the final betting round")(pending)
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

  private def playNextRound = {
    table = table.nextRound
    val player1 = table betters (0)
    val player2 = table betters (1)
    val player3 = table betters (2)
    table = table.place(player1.call).place(player2.check).place(player3.call)
    this
  }
}