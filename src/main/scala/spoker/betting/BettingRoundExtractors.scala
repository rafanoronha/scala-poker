package spoker.betting

private[betting] trait BettingRoundExtractors { this: BettingRound =>

  object OtherThanInTurn {
    def unapply(b: Better) = inTurn match {
      case Some(x) => b != x
      case None => true
    }
  }

  object SmallBlind {
    def unapply(b: Better) = b == smallBlind
  }

  object BigBlind {
    def unapply(b: Better) = b == bigBlind
  }

  object NonBigBlind {
    def unapply(b: Better) = b != bigBlind
  }

  object OtherThanFold {
    def unapply(a: Action) = Fold != a
  }

}