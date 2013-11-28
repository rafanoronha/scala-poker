package spoker.betting

private object BettingRoundExtractors {
  object BigBlind {
    def unapply(b: Better) = Position.BigBlind == b.position
  }

  object SmallBlind {
    def unapply(b: Better) = Position.SmallBlind == b.position
  }

  object NonBigBlind {
    def unapply(b: Better) = Position.BigBlind != b.position
  }

  object OtherThanFold {
    def unapply(a: Action) = Fold != a
  }

}