package spoker.betting

trait PlayersPositioning {

  val players: Seq[PositionedPlayer]

  def button: PositionedPlayer = players.find(isButton).get

  def smallBlind: PositionedPlayer = startingToTheLeftOfButton.head

  def bigBlind: PositionedPlayer = continually(startingAtButton).drop(2).head
  
  def bettersToAct = this

  def startingAtButton: Seq[PositionedPlayer] =
    players.dropWhile(isNotButton).takeWhile(_ => true) ++ players.takeWhile(isNotButton)

  def startingToTheLeftOfButton: Seq[PositionedPlayer] =
    continually(startingAtButton).drop(1).take(players.size)

  def startingToTheLeftOfBigBlind: Seq[PositionedPlayer] =
    continually(startingAtButton).drop(3).take(players.size)

  private def isButton(pp: PositionedPlayer) = pp.isButton

  private def isNotButton(pp: PositionedPlayer) = !pp.isButton

  private def continually(ps: Seq[PositionedPlayer]): Stream[PositionedPlayer] = Stream.continually(ps).flatten

}