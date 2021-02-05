package chess.pieces

import chess._

sealed abstract class RookSpec(val attacker: Colour) extends NonDirectionalPieceSpec(Rook) {

  override def vectors: List[(Int, Int)] = List(
    // format: off
    (-7,  0), (-6,  0), (-5,  0), (-4,  0), (-3,  0), (-2,  0), (-1,  0), //Horizontal part 1
    ( 1,  0), ( 2,  0), ( 3,  0), ( 4,  0), ( 5,  0), ( 6,  0), ( 7,  0), //Horizontal part 2
    ( 0, -7), ( 0, -6), ( 0, -5), ( 0, -4), ( 0, -3), ( 0, -2), ( 0, -1), //Vertical part 1
    ( 0,  1), ( 0,  2), ( 0,  3), ( 0,  4), ( 0,  5), ( 0,  6), ( 0,  7), //Vertical part 2
    // format: on
  )

  override def moves(position: Position): Set[Position] = position.rookMoves

  override def attacksWhileSurrounded(position: Position): Set[Position] = position.adjacent
}

class WhiteRookSpec extends RookSpec(White)
class BlackRookSpec extends RookSpec(Black)