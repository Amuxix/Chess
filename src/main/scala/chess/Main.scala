package chess

import cats.data.EitherT
import cats.effect.{IO, IOApp}

import scala.io.StdIn.readLine

object Main extends IOApp.Simple {

  //Check cats Resource loop method for weird continue thing.
  def getNewBoard(game: Game): IO[Game] = {
    def continue(newGame: EitherT[IO, Error, Game]): IO[Game] = retry(newGame)
    def retry(newGame: EitherT[IO, Error, Game]): IO[Game] =
      newGame.valueOrF { error =>
        IO(readLine(s"${error.message}, please input new move: ")).flatMap { move =>
          continue(game.executeMove(move))
        }
      }
    IO(readLine(s"${game.initiative}'s move: ")).flatMap(move => retry(game.executeMoves(move)))
  }

  override def run: IO[Unit] = {
    def continue(board: IO[Game]): IO[Game] = go(board)
    def go(board: IO[Game]): IO[Game] =
      board.flatMap {
        case board if board.isDraw || board.isCheckMate => IO.pure(board)
        case board =>
          board.print(Set.empty) *> continue(getNewBoard(board))

      }

    go(IO.pure(Game.initial)).void
  }

  /*  import cats.syntax.flatMap._
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
  }*/

  //override def run: IO[Unit] = IO.println("\uD83E\uDF00")

  //override def run: IO[Unit] = spinner(200.millis).as(ExitCode.Success)

  /*override def run: IO[Unit] =
    Board.initial.executeMoves("e4", "d5", "exd5").fold(error => IO.println(error), _ => IO.unit)*/

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
