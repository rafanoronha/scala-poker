package spoker.spec

import scala.io.Source
import scala.xml.XML
import scala.util.Try
import org.scalatest.FunSpec
import spoker._
import spoker.betting._
import scala.xml.Node
import org.scalatest.Matchers

class RoboPokerSpec extends FunSpec with Matchers {
  describe("The given scenario") {
    it("should pass") {
      val file = sys.props.get("robopokerscenario")
      file.foreach(f => {
        val source = (Source.fromFile(f, "utf-8"))
        val scenario = XML.loadString(source.getLines.mkString)
        val players = {
          val button = (scenario \\ "table" \ "@button").text
          def isNotButton(n: Node) = (n \ "@sit").text != button
          val ps = (scenario \\ "table" \\ "player").span(isNotButton)
          (ps._2 ++ ps._1).map(p => (Player((p \ "@name").text), (p \ "@stack").text.toInt))
        }
        val blinds = {
          val posts = (scenario \\ "post").map(b => (b \ "@amount").text.toInt)
          Blinds(posts.head, posts.tail.head)
        }
        val rounds = (scenario \\ "round").map(r => (r \\ "action")).filterNot(_.isEmpty).map(_.map(
          a => {
            val betterName = (a \ "@player").text
            val action = (a \ "@type").text match {
              case "fold" => Fold
              case "check" => Check
              //case "bet" =>
              case "call" => Call
              case "raise" => Raise(20)//Raise((a \ "@amount").text.toInt)
              //case "allin" =>
            }
            (action, betterName)
          }))
        println(blinds)
        println(players)
        println(rounds)
        //var table = Table(players.map(_._1), blinds).newHand
        var table = Table(Nil)
        rounds.foreach(r => {
          r.foreach(a => {
            val betterAction: (Action, Better) = (a._1, table.currentRound.get.players.find(_.manageablePlayer.name == a._2).get)
            println("placing action " + betterAction)
            table.place(betterAction)
          })
          if (!(rounds.head :: rounds.last :: Nil).contains(r))
            table = table.nextRound
        })
        players.foreach(p => {
          val better = table.players.find(_.player == p._1).get
          p._2 should be(table.stackManager.getPlayerStack(better))
        })
      })
    }
  }
}