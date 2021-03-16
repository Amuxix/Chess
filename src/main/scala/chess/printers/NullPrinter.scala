package chess.printers

import cats.effect.IO
import chess.{Game, Position}
import chess.printers.GamePrinter.PrinterFactory

case object NullPrinter extends GamePrinter[Unit] {
  implicit val factory: PrinterFactory[Unit] = (_: Game) => this

  override def withHighlights(highlightedPositions: Set[Position]): GamePrinter[Unit] = this

  override def withIcons: GamePrinter[Unit] = this

  override def withHeader: GamePrinter[Unit] = this

  override def withFooter(footer: String): GamePrinter[Unit] = this

  override def withMoves: GamePrinter[Unit] = this

  override def create: Unit = ()

  override def show: IO[Unit] = IO.unit
}
