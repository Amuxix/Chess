package chess

import chess.printers.AsciiPrinter
import chess.printers.GamePrinter.PrinterFactory
import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

trait EitherValues {
  implicit val printerFactory: PrinterFactory[String] = AsciiPrinter.factory

  implicit def toEitherValue[R](either: Either[Error, R])(implicit pos: source.Position): EitherValue[R] =
    new EitherValue(either, pos)

  class EitherValue[R](either: Either[Error, R], pos: source.Position) {

    def leftValue: Error = either.fold(
      identity,
      _ => throw new TestFailedException((_: StackDepthException) => Some("Either was right!"), None, pos),
    )

    def rightValue: R = either.fold(
      {
        case error: Error with HighlightedPositions =>
          throw new TestFailedException((_: StackDepthException) => Some(error.boardWithError), None, pos)
        case error: Error =>
          throw new TestFailedException((_: StackDepthException) => Some(error.message), None, pos)
      },
      identity,
    )

    def isRightOrThrow: Boolean = {
      rightValue
      true
    }

    def isLeftOrThrow: Boolean = {
      leftValue
      true
    }
  }
}
