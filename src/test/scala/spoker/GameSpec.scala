package spoker

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class GameSpec extends FunSpec with ShouldMatchers {

  it("should understand royal flush") {
    Game(Seq((Ace,Clubs),(King,Clubs),(Queen,Clubs),(Jack,Clubs),(Ten,Clubs))).isInstanceOf[RoyalFlush].should(be(true))
  }
}
