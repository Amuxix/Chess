package chess.pieces

import chess._

class BishopSpec extends NonDirectionalPieceSpec(Bishop) {

  override def attacker: Colour = White

  override def vectors: List[(Int, Int)] = List(
    // format: off
    (-7, -7), (-6, -6), (-5, -5), (-4, -4), (-3, -3), (-2, -2), (-1, -1), //Bottom left diagonal
    (-7,  7), (-6,  6), (-5,  5), (-4,  4), (-3,  3), (-2,  2), (-1,  1), //Top left diagonal
    ( 1,  1), ( 2,  2), ( 3,  3), ( 4,  4), ( 5,  5), ( 6,  6), ( 7,  7), //Top right diagonal
    ( 1, -1), ( 2, -2), ( 3, -3), ( 4, -4), ( 5, -5), ( 6, -6), ( 7, -7), //Bottom right diagonal
    // format: on
  )

  override def moves(position: Position): Set[Position] = position.diagonals

  override def attacksWhileSurrounded(position: Position): Set[Position] = position.diagonalsAround
}
