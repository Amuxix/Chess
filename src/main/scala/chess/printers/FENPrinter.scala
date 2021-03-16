package chess.printers

import cats.effect.IO
import chess._
import chess.printers.GamePrinter.PrinterFactory

case class FENPrinter(game: Game) extends GamePrinter[String] {
  override def withHighlights(highlightedPositions: Set[Position]): GamePrinter[String] = this

  override def withIcons: GamePrinter[String] = this

  override def withHeader: GamePrinter[String] = this

  override def withFooter(footer: String): GamePrinter[String] = this

  override def withMoves: GamePrinter[String] = this

  private def pieceString(piece: Piece): String = {
    val string = AlgebraicNotation.pieceString(piece)
    piece.colour.perColour(string.toUpperCase)(string.toLowerCase)
  }

  override def create: String = {
    val lines = Position.all
      .grouped(8)
      .map { line =>
        line
          .map(game.pieceAt(_).toOption)
          .foldLeft(("", 0)) {
            case ((_, emptyPositions), None) if emptyPositions == 7 => // Completely empty line
              ("8", 0)
            case ((lineString, emptyPositions), None) =>
              (lineString, emptyPositions + 1)
            case ((lineString, emptyPositions), Some(piece)) =>
              val empty = if (emptyPositions > 0) emptyPositions.toString else ""
              (lineString + empty + pieceString(piece), 0)
          }
          ._1
      }
      .mkString("/")
    val initiative = game.initiative.toString.take(1).toLowerCase
    val castles = {
      val whiteKing = Option.when(!game.findKing(White).hasMoved)(())
      val blackKing = Option.when(!game.findKing(Black).hasMoved)(())
      val castles = List(
        whiteKing.flatMap(_ => Option.when(game.pieceAt(A1).exists(!_.hasMoved))("K")),
        whiteKing.flatMap(_ => Option.when(game.pieceAt(H1).exists(!_.hasMoved))("Q")),
        blackKing.flatMap(_ => Option.when(game.pieceAt(A8).exists(!_.hasMoved))("k")),
        blackKing.flatMap(_ => Option.when(game.pieceAt(H8).exists(!_.hasMoved))("q")),
      ).flatten
      if (castles.isEmpty) "-" else castles.mkString
    }
    val enPassant = game.lastMove
      .flatMap {
        case (pawn: Pawn, target) if !pawn.hasMoved && math.abs(pawn.position.rankInt - target.rankInt) == 2 =>
          pawn.colour.perColour(pawn.position + (0, 1))(pawn.position + (0, -1)).map(_.algebraicNotation)
        case _ => None
      }
      .getOrElse("-")
    s"$lines $initiative $castles $enPassant 0 0"
  }

  override def show: IO[Unit] = IO.println(create)
}

object FENPrinter {
  implicit val factory: PrinterFactory[String] = (game: Game) => FENPrinter(game)
}
