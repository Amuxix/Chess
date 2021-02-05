package chess.pieces

import chess._
import org.scalatest.enablers.Emptiness.emptinessOfGenTraversable

class BlackPawnSpec extends PieceSpec(Pawn) {
  override def attacker: Colour = Black

  s"A Black ${pieceType.className}" should {
    "have correct vector moves if not moved" ignore {
      //Pawn(Black).vectorMoves should contain only ((0, -1), (0, -2))
    }
    "have correct vector moves if already moved" ignore {
      //pieceType.vectorMoves should contain only ((0, -1))
    }
    "have correct vector captures" ignore {
      //pieceType.vectorCaptures should contain only ((1, -1), (-1, -1))
    }
    /*"have correct possible moves if in center of board" in {
      possibleMoves(boardWithPiece, possibleKings, D4) should contain only D3
    }
    "have no possible moves if in bottom edge" in {
      possibleMoves(boardWithPiece, possibleKings, D1) shouldBe empty
    }
    "have correct possible moves if on the top edge" in {
      possibleMoves(boardWithPiece, possibleKings, D8) should contain only D7
    }
    "have correct possible moves if on the left edge" in {
      possibleMoves(boardWithPiece, possibleKings, A4) should contain only A3
    }
    "have correct possible moves if on the right edge" in {
      possibleMoves(boardWithPiece, possibleKings, H4) should contain only H3
    }
    "have no possible moves if surrounded" in {
      possibleMoves(boardWithPieceSurroundedByEnemies, possibleKings, D4) shouldBe empty
    }*/
  }
}
