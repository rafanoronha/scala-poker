package spoker.table.spec

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import spoker.table._
import spoker.betting.RoundKind._
import spoker.betting.Better

class TableSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  var table: Table = null
  var betters: Seq[Better] = null

  before {
    table = Table(new Player("p1") :: new Player("p2") :: new Player("p3") :: Nil).newHand
    betters = table betters
  }

  def player1 = table.betters find {
    betters(0) == _.player
  } get

  def player2 = table.betters find {
    betters(1) == _.player
  } get

  def player3 = table.betters find {
    betters(2) == _.player
  } get

  def playNextRound = {
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
    // TODO: make it green
    it("should be won by unmatched bet owner") {
      table = table.place(player1.raise(4)).place(player2.call).place(player3.fold)
        .nextRound.place(player1.raise(8)).place(player2.fold)
      (player1 stack) should be(58)
    }
    it("should collect the blinds") {
      table.pot.get.stack should be(3)
    }
    // TODO: make it green
    it("should collect all played stakes") {
      table = table.place(player1.raise(4)).place(player2.call).place(player3.fold)
        .nextRound.place(player1.raise(8)).place(player2.fold)
      (table.pot.get stack) should be(12)
    }
  }

}