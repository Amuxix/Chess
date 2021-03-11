package chess

sealed abstract class Move {
  def mustEndInCheck: Boolean

  def mustEndInMate: Boolean

  def move: String
}

case class SimpleMove(piece: Piece, target: Position, mustEndInCheck: Boolean, mustEndInMate: Boolean, move: String)
    extends Move

case class Capture(piece: Piece, target: Piece, mustEndInCheck: Boolean, mustEndInMate: Boolean, move: String)
    extends Move

case class EnPassantCapture(
  piece: Piece,
  target: Position,
  targetPiece: Piece,
  mustEndInCheck: Boolean,
  mustEndInMate: Boolean,
  move: String,
) extends Move

case class KingSideCastle(mustEndInCheck: Boolean, mustEndInMate: Boolean, move: String) extends Move

case class QueenSideCastle(mustEndInCheck: Boolean, mustEndInMate: Boolean, move: String) extends Move

case class Promotion(
  pawn: Pawn,
  target: Position,
  promotion: PieceType,
  mustEndInCheck: Boolean,
  mustEndInMate: Boolean,
  move: String,
) extends Move

case class CapturePromotion(
  pawn: Pawn,
  target: Piece,
  promotion: PieceType,
  mustEndInCheck: Boolean,
  mustEndInMate: Boolean,
  move: String,
) extends Move
