package chess

case class State(
  initiative: Colour, // Who has the right to move
  board: Board,
  lastMove: Option[(Piece, Position)] = None,
) {
  def updateBoard(f: Board => Board): State = copy(board = f(board))
  lazy val withSwappedInitiative: State = copy(initiative = initiative.other)
}
