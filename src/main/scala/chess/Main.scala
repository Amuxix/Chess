package chess

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn.readLine
import scala.util.Random

object Main extends IOApp.Simple {

  //Check cats Resource loop method for weird continue thing.
  /*def getNewBoard(board: Board): IO[Board] = {
    def continue(newBoard: Either[Error, Board]): IO[Board] = retry(newBoard)
    def retry(newBoard: Either[Error, Board]): IO[Board] =
      newBoard match {
        case Left(error) =>
          IO(readLine(s"$error, please input new move: ")).flatMap { move =>
            continue(board.executeMoves(move))
          }
        case Right(value) => IO.pure(value)
      }
    IO(readLine(s"${board.initiative}'s move: ")).flatMap(move => retry(board.executeMoves(move)))
  }*/

  /*override def run(args: List[String]): IO[ExitCode] = {
    def continue(board: IO[Board]): IO[Board] = go(board)
    def go(board: IO[Board]): IO[Board] =
      board.flatMap {
        case board if board.isDraw || board.isCheckMate => IO.pure(board)
        case board =>
          board.print *> continue(getNewBoard(board))

      }

    go(IO.pure(Board.initial)).as(ExitCode.Success)
  }*/

  import cats.syntax.flatMap._
  import scala.concurrent.duration._

  def spinner(delay: FiniteDuration): IO[Unit] = {
    val spinners = List(
      //"▖▘▝▗",
      //"⣷⣯⣟⡿⢿⣻⣽⣾",
      "⣯⣟⡿⢿⣻⣽⣷⠈⠐⠠⢀⡀⠄⠂⠁",
      //"⠁⠈⠐⠠⢀⡀⠄⠂",
      //"▁▃▄▅▆▇█▇▆▅▄▃",
      //"◢◣◤◥",
      //"⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏",
    )
    val spinnerChars = spinners(Random.nextInt(spinners.length))
    0.iterateForeverM { number =>
      for {
        _ <- IO.print(s"\b${spinnerChars(number)}")
        _ <- IO.sleep(delay)
      } yield (number + 1) % spinnerChars.length
    }
  }

  //override def run: IO[Unit] = IO.println("\uD83E\uDF00")

  //override def run: IO[Unit] = spinner(200.millis).as(ExitCode.Success)

  /*override def run: IO[Unit] =
    Board.initial.executeMoves("e4", "d5", "exd5").fold(error => IO.println(error), _ => IO.unit)*/

  override def run: IO[Unit] =
    Game.initial.executeMoves("e4").fold(error => IO.println(error), _ => IO.unit)

  /*override def run(args: List[String]): IO[ExitCode] =
    IO {
      println(A1.squareColour)
      println(A2.squareColour)
      println(Knight.moves)
      println(Board.withPieces((D4 -> Knight(White))).possibleMoves(D4))
      println(Board.withPieces((D1 -> Knight(White))).possibleMoves(D1))
      println(Board.withPieces((D8 -> Knight(White))).possibleMoves(D8))
      println(Board.withPieces((A4 -> Knight(White))).possibleMoves(A4))
      println(Board.withPieces((H4 -> Knight(White))).possibleMoves(H4))
    }.as(ExitCode.Success)*/
}
