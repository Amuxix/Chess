package chess

import cats.syntax.traverse._
import cats.syntax.either._
import chess.AlgebraicNotation._

case class AlgebraicNotation(game: Game) {

  private def nonPawnMove(capture: Option[Piece], start: Piece, end: Piece): String = {
    val piecesThatCanMove =
      capture
        .fold(game.movers(end.position, end.colour))(_ => game.attackers(end.position, end.colour))
        .filter(_.pieceType == start.pieceType)

    lazy val sameFile = piecesThatCanMove.count(_.position.file == start.position.file)
    lazy val sameRank = piecesThatCanMove.count(_.position.rank == start.position.rank)
    val disambiguation = piecesThatCanMove.size match {
      case x if x == 1        => "" //No need for disambiguating
      case _ if sameFile == 1 => start.position.file
      case _ if sameRank == 1 => start.position.rank
      case _                  => start.position.algebraicNotation
    }
    s"${pieceString(start)}$disambiguation${capture.fold("")(_ => "x")}${end.position.algebraicNotation}"
  }

  private def pawnMove(capture: Option[Piece], start: Pawn, end: Piece): String =
    capture.fold(end.position.algebraicNotation) {
      case captured if captured.position != end.position =>
        s"${start.position.file}x${end.position.algebraicNotation} e.p."
      case _ =>
        s"${start.position.file}x${end.position.algebraicNotation}"
    }

  private def promotionMove(capture: Option[Piece], start: Pawn, end: Piece): String =
    s"${pawnMove(capture, start, end)}${pieceString(end)}"

  private def castleMove(movedPieces: List[Piece]): Either[Error, String] = {
    val kings = movedPieces.collect { case king: King => king.position }
    for {
      _ <- Either.cond(movedPieces.count(_.pieceType == Rook) == 2 && kings.size == 2, (), CouldNotIdentifyMove)
      move <- Either
        .cond(kings.exists(_.fileInt == 2), "O-O-O", CouldNotIdentifyMove)
        .orElse(Either.cond(kings.exists(_.fileInt == 6), "O-O", CouldNotIdentifyMove))
    } yield move
  }

  def findMove(nextGame: Game): Either[Error, String] = {
    lazy val pieceChanges = game.board.pieces.values.toSet symDiffList nextGame.board.pieces.values.toSet
    lazy val byColour = pieceChanges.groupBy(_.colour)
    for {
      capture <- byColour.get(game.initiative.other).traverse {
        case capture :: Nil => capture.asRight
        case _ :: _         => CouldNotIdentifyMove.asLeft //Two or more enemy pieces changed, wtf is this?
      }
      move <- byColour(game.initiative) match {
        case (start: Pawn) :: (end: Pawn) :: Nil =>
          pawnMove(capture, start, end).asRight

        case (start: Pawn) :: end :: Nil =>
          promotionMove(capture, start, end).asRight

        case start :: end :: Nil if start.pieceType == end.pieceType =>
          nonPawnMove(capture, start, end).asRight

        case castle if castle.length == 4 =>
          castleMove(castle)

        case _ =>
          CouldNotIdentifyMove.asLeft // Weird number of pieces changed
      }

      moveWithSuffix =
        if (nextGame.kingIsInCheck(nextGame.initiative)) {
          s"$move+"
        } else if (nextGame.isCheckMate) {
          s"$move#"
        } else {
          move
        }

    } yield moveWithSuffix
  }
}

object AlgebraicNotation {
  def findMove(game: Game, nextGame: Game): Either[Error, String] = AlgebraicNotation(game).findMove(nextGame)

  def findMove(game: Evaluator, nextGame: Evaluator): Either[Error, String] =
    AlgebraicNotation(game.game).findMove(nextGame.game)

  def pieceString(piece: Piece): String = piece match {
    case _: Pawn   => "P"
    case _: Knight => "N"
    case _: Bishop => "B"
    case _: Rook   => "R"
    case _: Queen  => "Q"
    case _: King   => "K"
  }

}
