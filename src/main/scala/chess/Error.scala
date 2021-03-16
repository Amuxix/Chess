package chess

import chess.printers.GamePrinter.PrinterFactory

sealed trait Error {
  val message: String
}

sealed trait HighlightedPositions { this: Error =>
  val game: Game
  val highlightedPositions: Seq[Position]

  def boardWithError(implicit pf: PrinterFactory[String]): String =
    pf(game).withHighlights(highlightedPositions.toSet).withIcons.withHeader.withFooter(message).create
}

case class AlgebraicNotationParseError(algebraicNotation: String) extends Error {
  override val message: String = s"Could not parse position from algebraic notation: $algebraicNotation"
}

case object MissingFile extends Error {
  override val message: String = "File not present"
}

case object MissingRank extends Error {
  override val message: String = "Rank not present"
}

case object IllegalMove extends Error {
  override val message: String = "Illegal Move"
}

case class PieceNotFound(position: Position) extends Error {
  override val message: String = s"Could not find piece at $position"
}

case class NoValidPieceToMove(move: String, game: Game, highlightedPositions: Position*)
    extends Error
    with HighlightedPositions {
  override val message: String = s"Could not find piece to perform move $move"
}

case class PieceCantMoveThatWay(game: Game, highlightedPositions: Position*) extends Error with HighlightedPositions {
  override val message: String = "Illegal move, piece can't move there"
}

case class TargetOccupied(target: Position, game: Game, highlightedPositions: Position*)
    extends Error
    with HighlightedPositions {
  override val message: String = s"There's a piece at the target $target"
}

case class ObstacleInTheWay(game: Game, highlightedPositions: Position*) extends Error with HighlightedPositions {
  override val message: String = "There's a piece in the way"
}

case class KingWouldEndUpInCheck(game: Game, highlightedPositions: Position*) extends Error {
  override val message: String = "Illegal move, your king is in check or becomes in check if you move"
}

case class TargetIsAllied(game: Game, highlightedPositions: Position*) extends Error {
  override val message: String = "Illegal move, can't capture your own pieces"
}

case object WrongInitiative extends Error {
  override val message: String = "Moving or Attacking piece colour is not the colour currently playing"
}

case class AlreadyOccupied(game: Game, highlightedPositions: Position*) extends Error with HighlightedPositions {
  override val message: String = "Piece already exists in that position"
}

case class RouteIsUnderAttack(game: Game, highlightedPositions: Position*) extends Error with HighlightedPositions {
  override val message: String = "A position between kings position and final position is under attack"
}

case class KingAlreadyMoved(king: King) extends Error {
  override val message: String = s"King have already moved $king"
}

case class RookAlreadyMoved(piece: Piece) extends Error {
  override val message: String = s"Rook not in initial position, found $piece"
}

case object AmbiguousPromotion extends Error {
  override val message: String = "Ambiguous pawn to promote"
}

case class MoveIsNotCheck(game: Game, highlightedPositions: Position*) extends Error with HighlightedPositions {
  override val message: String = "Move did not result in a check"
}

case class MoveIsNotMate(game: Game, highlightedPositions: Position*) extends Error with HighlightedPositions {
  override val message: String = "Move did not result in a checkmate"
}

case object EnPassantNotPossible extends Error {
  override val message: String = "En passant not possible"
}

case object InvalidLastMovedPieceForEnPassant extends Error {
  override val message: String = "Piece moved last turn was not a Pawn"
}

case object InvalidTargetForEnPassant extends Error {
  override val message: String = "Target piece must be the Pawn that just moved"
}

case object InvalidCapturingPiece extends Error {
  override val message: String = "Only Pawns can capture en passant"
}

case class WrongRankForEnPassant(game: Game, highlightedPositions: Position*) extends Error with HighlightedPositions {
  override val message: String = "The capturing pawn must be on its fifth rank"
}

case class WrongFileForEnPassant(game: Game, highlightedPositions: Position*) extends Error with HighlightedPositions {
  override val message: String = "The captured pawn must be on an adjacent file"
}

case class NoPreviousDoubleStepMoveEnPassant(game: Game, highlightedPositions: Position*)
    extends Error
    with HighlightedPositions {
  override val message: String = "The captured pawn must have just moved two squares in a single move"
}

case object CouldNotIdentifyMove extends Error {
  override val message: String = "Could not identify move"
}
