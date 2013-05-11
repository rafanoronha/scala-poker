package spoker

import Rank._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class GameSpec extends FunSpec with ShouldMatchers {

  it("should understand royal flush") {
    Game(Seq((Ace,Clubs),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))).isInstanceOf[RoyalFlush].should(be(true))
  }
  it("should understand a game irrespective of order of the cards") {
    Game(Seq((Ace,Clubs),(Queen,Clubs),(King,Clubs),(Ten,Clubs),(Jack,Clubs))).isInstanceOf[RoyalFlush].should(be(true))
  }
  it("should understand straight flush") {
    Game(Seq((Six,Diamonds),(Seven,Diamonds),(Eight,Diamonds),(Nine,Diamonds),(Ten,Diamonds))).isInstanceOf[StraightFlush].should(be(true))
  }
}
