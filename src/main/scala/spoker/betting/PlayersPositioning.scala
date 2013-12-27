package spoker.betting

import spoker.ManageablePlayer

trait PlayersPositioning {

  val players: Seq[ManageablePlayer]

  def button: ManageablePlayer = players.find(isButton).get

  def smallBlind: ManageablePlayer = startingToTheLeftOfButton.head

  def bigBlind: ManageablePlayer = continually(startingAtButton).drop(2).head

  def bettersToAct = this

  def startingAtButton: Seq[ManageablePlayer] =
    players.dropWhile(isNotButton).takeWhile(_ => true) ++ players.takeWhile(isNotButton)

  def startingToTheLeftOfButton: Seq[ManageablePlayer] =
    continually(startingAtButton).drop(1).take(players.size)

  def startingToTheLeftOfBigBlind: Seq[ManageablePlayer] =
    continually(startingAtButton).drop(3).take(players.size)

  private def isButton(p: ManageablePlayer) = p.isButton

  private def isNotButton(p: ManageablePlayer) = !p.isButton

  private def continually(ps: Seq[ManageablePlayer]): Stream[ManageablePlayer] = Stream.continually(ps).flatten

}