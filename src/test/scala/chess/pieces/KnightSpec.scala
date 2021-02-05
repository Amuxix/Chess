package chess.pieces

import chess._

sealed abstract class KnightSpec(val attacker: Colour) extends NonDirectionalPieceSpec(Knight) {

  override def vectors: List[(Int, Int)] =
    List((-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1))

  override def moves(position: Position): Set[Position] = position.knightMoves

  override def attacksWhileSurrounded(position: Position): Set[Position] = moves(position)
}

class WhiteKnightSpec extends KnightSpec(White)
class BlackKnightSpec extends KnightSpec(Black)
