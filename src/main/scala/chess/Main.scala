package chess

import cats.data.EitherT
import cats.effect.{IO, IOApp, Resource}
import cats.effect.implicits._
import cats.syntax.parallel._
import cats.syntax.flatMap._
import cats.effect.unsafe.{IORuntime, Scheduler}
import chess.printers.ImageGamePrinter.factory
import chess.printers.ImageGamePrinter

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import java.util.concurrent.{Executors, ThreadFactory}
import scala.concurrent.ExecutionContext
import scala.io.StdIn.readLine
import scala.util.Random
import scala.concurrent.duration._

object Main extends IOApp.Simple {

  /*override protected val runtime: IORuntime = {
    val (scheduler, schedDown) = Scheduler.createDefaultScheduler()
    val threadCount = new AtomicInteger(0)
    IORuntime(
      compute = ExecutionContext.fromExecutor(
        Executors.newFixedThreadPool(
          2,
          (r: Runnable) => {
            val t = new Thread(r)
            t.setName(s"batata-${threadCount.getAndIncrement()}")
            t.setDaemon(true)
            t
          },
        ),
      ),
      blocking = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1)),
      scheduler = scheduler,
      schedDown,
    )
  }

  val printThread: IO[Unit] = IO.println(s"I'm running on ${Thread.currentThread().getName}")

  def task(s: String)(implicit ec: ExecutionContext): IO[Unit] =
    (IO.print(s + " ") *> IO.sleep(Random.nextInt(2).second) *> printThread).evalOn(ec)

  def cpuBoundTask: IO[Unit] = IO {
    println(Thread.currentThread().getName)
    (for {
      a <- List.range(1, Int.MaxValue)
      _ = println(Thread.currentThread().getName)
      b <- List.range(1, Int.MaxValue)
      _ = println(Thread.currentThread().getName)
      c <- List.range(1, Int.MaxValue)
    } yield a * b * c).sum
  }

  lazy val computationThreadPool: Resource[IO, ExecutionContext] =
    Resource(IO {
      val executor = Executors.newFixedThreadPool(2)
      val ec = ExecutionContext.fromExecutor(executor)
      (ec, IO(executor.shutdown()))
    })

  //override def run: IO[Unit] = computationThreadPool.use(ec => task.evalOn(ec))
  /*override def run: IO[Unit] = computationThreadPool.use { ec =>
    for {
      _ <- task(runtime.compute).start
      fiber <- List.fill(10)(task(ec)).parSequence_.start
      _ <- fiber.join
    } yield ()
  }*/
  override def run: IO[Unit] = {
    val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))
    println("Main: " + Thread.currentThread().getName)
    for {
      _ <- task("First")(runtime.compute)
      _ <- cpuBoundTask.evalOn(ec).start
      _ <- task("Second")(runtime.compute)
    } yield ()
  }*/

  implicit class IOOps[A](val io: IO[A]) extends AnyVal {

    def withSpinner(delay: FiniteDuration): IO[A] =
      spinner(delay).race(io).map(_.getOrElse(throw new Exception))
  }

  def spinner(delay: FiniteDuration): IO[Unit] = {
    val spinners = List(
      "▖▘▝▗",
      "⣷⣯⣟⡿⢿⣻⣽⣾",
      "⠁⠈⠐⠠⢀⡀⠄⠂",
      "▁▃▄▅▆▇█▇▆▅▄▃",
      "◢◣◤◥",
      "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏",
    )
    val spinnerChars = spinners(Random.nextInt(spinners.length))
    0.iterateForeverM { number =>
      for {
        _ <- IO.print(s"\b${spinnerChars(number)}")
        _ <- IO.sleep(delay)
      } yield (number + 1) % spinnerChars.length
    }
  }

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
    def continue(game: IO[Game], playerColour: Colour): IO[Game] = go(game, playerColour)
    def go(game: IO[Game], playerColour: Colour): IO[Game] = for {
      nextGame <- game.flatMap {
        case game if game.isDraw || game.isCheckMate => IO.pure(game)
        case game if game.initiative == playerColour =>
          continue(getNewBoard(game), playerColour)
        case game =>
          Evaluator(game).bestMove(45.seconds).flatMap {
            case Some(evaluator) => continue(IO.pure(evaluator.game), playerColour)
            case None            => IO.pure(game) //Evaluator couldn't find a move, this shouldn't ever be reached
          }
      }
    } yield nextGame

    ImageGamePrinter.resource.use { _ =>
      //val playerColour = List(White, Black)(Random.nextInt(2))
      val playerColour = White
      val game = Game.initial
      //val game = Parser.gameFromFEN("1rb2bnr/pppp1kpp/2n1p3/4P1q1/5P2/8/PPPP2PP/RNBQ1K1R b - f3 0 8").get
      //val game = Parser.gameFromFEN("8/8/8/8/3K4/8/r7/1q5k b - - 0 1").get //Ladder mate
      val initial = game.show(Set.empty).as(game)
      IO.println(s"Player is $playerColour") *> go(initial, playerColour).void
    }
  }

  /*override def run: IO[Unit] =
    ImageGamePrinter.resource.use { _ =>
      //val game = Parser.gameFromFEN("Q4bkr/4p1p1/8/5pNp/5B2/8/PPR2qPP/2K1R3 b - - 3 21").get //Mate in 1
      //val game = Parser.gameFromFEN("Q4bkr/6p1/8/4ppNp/5B2/8/PPK3PP/4R3 w - e6 0 23").get //Mate in 1
      //val game = Parser.gameFromFEN("r4r1k/pq1n2pp/4Q2N/1p6/3n1P2/8/2P4P/2K3R1 w - - 0 24").get //Mate in 2
      //val game = Parser.gameFromFEN("r1b2rk1/ppp2p1p/1b1p1B2/5q1Q/2Bp4/2P5/PP3PPP/R3R1K1 w - - 0 1").get //Mate in 2
      val game = Parser.gameFromFEN("8/8/8/8/3K4/8/r7/1q5k b - - 0 1").get
      //val game = Game.initial
      Evaluator(game).bestMove(60.seconds).void
    }*/

  /*def spinner(delay: FiniteDuration): IO[Unit] = {
    //🭗🭁🭘🭂🭙🭃🭚🭄🭜🭆🭛🭅🭡🭋🭟🭉🭠🭊🭞🭈🭝🭇
    //🬼🭒🬽🭓🬾🭔🬿🭕🭑🭧🭀🭖🭐🭦🭎🭤🭏🭥🭍🭣🭌🭢
    val spinners = List(
      "🕛🕐🕑🕒🕓🕔🕕🕖🕗🕘🕙🕚",
      "🕧🕜🕝🕞🕟🕠🕡🕢🕣🕤🕥",
      "🌕🌔🌓🌒🌑🌘🌗🌖",
      "🙂😀😃😁😃😀",
      "🖖🤟🖐🤘🤞",
      "📪📫📬📭",
      "👆👉👇👈",
      "🔈🔉🔊🔉",
      "🔅🔆",
      "🔲🔳",
      "📀💿",
      "▖▘▝▗",
      "◢◣◤◥",
      "🭭🭮🭯🭬",
      "🭩🭪🭫🭨",
      "⠁⠈⠐⠠⢀⡀⠄⠂⠁⠉⠘⠰⢠⣀⡄⠆⠃⠉⠙⠸⢰⣠⣄⡆⠇⠋⠙⠹⢸⣰⣤⣆⡇",
      "▁▃▄▅▆▇█🮆🮅🮄🮃🮂🮃🮄🮅🮆█▇▆▅▄▃",
      "▏▎▍▋▊▉█🮋🮊🮉🮈🮇🮈🮉🮊🮋█▉▊▋▍▎",
      "🮃🭧🭕🭔🭖🮉🭅🭄🭆▄🭑🭏🭐🭡▋🭛🭟🭠🭜",
      "🭗🭘🭜🭞🭝█🭁🭂🭆🭈🭇🭈🭆🭂🭁█🭝🭞🭜",
      "🭙🭛🭡🭟█🭃🭅🭋🭉🭋🭅🭃█🭟🭡🭛",
      "▏🭰🭱🭲🭳🭴🭵▕🭵🭴🭳🭲🭱🭰",
      "🮠🮡🮣🮢🮠🮧🮥🮦🮤🮬🮪🮫🮭🮮",
      "🭶🭷🭸🭹🭺🭻🭺🭹🭸🭷",
      "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏",
      "⣷⣯⣟⡿⢿⣻⣽⣾",
    )
    val spinner = spinners(Random.nextInt(spinners.length))
    val codePoints = spinner.codePoints().toArray
    0.iterateForeverM { number =>
      val chars = Character.toChars(codePoints(number))
      val clearSpinner = "\b" * chars.length
      val io = for {
        _ <- IO.print(clearSpinner + new String(chars))
        _ <- IO.sleep(delay)
      } yield (number + 1) % codePoints.length
      io.onCancel(IO.print(clearSpinner))
    }
  }*/
}
