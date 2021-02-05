package chess.pieces

import chess._
import org.scalatest.enablers.Emptiness.emptinessOfGenTraversable

class KingSpec extends PieceSpec(King) {
  //TODO Maybe use NonDirectional here as well?
  override def attacker: Colour = White

  s"A ${pieceType.className}" should {
    "have correct vector moves" ignore {
      /*pieceType.vectorMoves should contain only (
        (1, -1), (1, 0), (1, 1),
        (0, -1), (0, 1),
        (-1, -1), (-1, 0), (-1, 1),
      )*/
    }
    "have correct vector captures" ignore {
      /*pieceType.vectorCaptures should contain only (
        (1, -1), (1, 0), (1, 1),
        (0, -1), (0, 1),
        (-1, -1), (-1, 0), (-1, 1),
      )*/
    }
    /*"have correct possible moves if in center of board" in {
      possibleMoves(boardWithPiece, possibleKings, D4) should contain only (C3, D3, E3, C4, E4, C5, D5, E5)
    }
    "have correct possible moves if in bottom edge" in {
      possibleMoves(boardWithPiece, possibleKings, D1) should contain only (C1, E1, C2, D2, E2)
    }
    "have correct possible moves if in top edge" in {
      possibleMoves(boardWithPiece, possibleKings, D8) should contain only (C7, D7, E7, C8, E8)
    }
    "have correct possible moves if in left edge" in {
      possibleMoves(boardWithPiece, possibleKings, A4) should contain only (A3, B3, B4, A5, B5)
    }
    "have correct possible moves if in right edge" in {
      possibleMoves(boardWithPiece, possibleKings, H4) should contain only (G3, H3, G4, G5, H5)
    }
    "have no possible moves if surrounded" in {
      possibleMoves(boardWithPieceSurroundedByEnemies, possibleKings, D4) shouldBe empty
    }*/
  }
}
