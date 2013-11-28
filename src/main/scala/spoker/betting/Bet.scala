package spoker.betting

case class Bet(
  value: Int,
  placedBy: Better,
  bettersToAct: Iterator[Better],
  matchedBy: Seq[Better] = Nil)
