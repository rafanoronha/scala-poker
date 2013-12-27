package spoker.betting

case class Bet(
  value: Int = 0,
  placedBy: Better = null,
  bettersToAct: Iterator[Better],
  matchedBy: Seq[Better] = Nil)
