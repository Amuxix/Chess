package chess.printers

import cats.effect.IO
import chess.{Game, Position}

abstract class GamePrinter[T] {
  def withHighlights(highlightedPositions: Set[Position]): GamePrinter[T]

  def withIcons: GamePrinter[T]

  def withHeader: GamePrinter[T]

  def withFooter(footer: String): GamePrinter[T]

  def withMoves: GamePrinter[T]

  def create: T

  def show: IO[Unit]
}

object GamePrinter {
  type PrinterFactory[T] = Game => GamePrinter[T]
}
