package chess.pieces

import chess._

sealed abstract class QueenSpec(val attacker: Colour) extends NonDirectionalPieceSpec(Queen) {

  override def vectors: List[(Int, Int)] = List(
    // format: off
    (-7,  0), (-6,  0), (-5,  0), (-4,  0), (-3,  0), (-2,  0), (-1,  0), //Horizontal part 1
    ( 1,  0), ( 2,  0), ( 3,  0), ( 4,  0), ( 5,  0), ( 6,  0), ( 7,  0), //Horizontal part 2
    ( 0, -7), ( 0, -6), ( 0, -5), ( 0, -4), ( 0, -3), ( 0, -2), ( 0, -1), //Vertical part 1
    ( 0,  1), ( 0,  2), ( 0,  3), ( 0,  4), ( 0,  5), ( 0,  6), ( 0,  7), //Vertical part 2
    (-7, -7), (-6, -6), (-5, -5), (-4, -4), (-3, -3), (-2, -2), (-1, -1), //Bottom left diagonal
    (-7,  7), (-6,  6), (-5,  5), (-4,  4), (-3,  3), (-2,  2), (-1,  1), //Top left diagonal
    ( 1,  1), ( 2,  2), ( 3,  3), ( 4,  4), ( 5,  5), ( 6,  6), ( 7,  7), //Top right diagonal
    ( 1, -1), ( 2, -2), ( 3, -3), ( 4, -4), ( 5, -5), ( 6, -6), ( 7, -7), //Bottom right diagonal
    // format: on
  )

  override def moves(position: Position): Set[Position] = position.queenMoves

  override def attacksWhileSurrounded(position: Position): Set[Position] = position.allAround
}

class WhiteQueenSpec extends QueenSpec(White)
class BlackQueenSpec extends QueenSpec(Black)