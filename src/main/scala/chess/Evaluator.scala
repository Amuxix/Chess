package chess

import cats.Order
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.either._
import cats.syntax.parallel._
import cats.syntax.foldable._
import cats.syntax.traverse._
import chess.Evaluator._
import chess.printers.{AsciiPrinter, FENPrinter}
import chess.printers.GamePrinter.PrinterFactory

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.Random
import scala.util.chaining._

case class Evaluator(
  game: Game,
  transpositionTable: Map[State, (Int, Int)] = Map.empty,
) {

  private lazy val colourMultiplier = game.initiative.perColour(1)(-1)

  lazy val evaluation: Int =
    if (game.isCheckMate) { //Player with initiative was checkmated
      checkmateEvaluation
    } else if (game.withSwappedInitiative.isCheckMate) {
      -checkmateEvaluation
    } else if (game.isDraw) {
      0
    } else {
      //lazy val whitePieceValue = game.colourPieces(White).sumBy(_.captureValue)
      //lazy val blackPieceValue = game.colourPieces(Black).sumBy(_.captureValue)
      //lazy val pointsOffBoard = (totalBoardPoints - whitePieceValue - blackPieceValue) / 1000
      def attackers(colour: Colour) = Position.all.foldLeft(Map.empty[Position, Set[Piece]]) {
        case (attackers, position) => attackers + ((position, game.attackers(position, colour)))
      }

      def pointsByColour(colour: Colour): Int = {
        //println(s"Evaluating $colour pieces")
        lazy val thisAttackers = attackers(colour)
        lazy val otherAttackers = attackers(colour.other)
        lazy val aroundOpposingKing = game.findKing(colour.other).position.allAround
        game.colourPieces(colour).foldLeft(0) { case (total, piece) =>
          //println(s"  $piece capture value ${piece.captureValue}")
          total + (piece.captureValue + game.threatenedPositions(piece).toList.foldLeft(0) { case (total, target) =>
            lazy val positionWeight = 5
            lazy val emptySquareValue = piece match {
              case _: King                                  => positionWeight
              case _ if aroundOpposingKing.contains(target) => 500
              case _                                        => positionWeight
            }
            lazy val attackerDifference =
              (thisAttackers(target).size - 1, otherAttackers(target).size) match {
                case (ours, others) if ours >= 1 && ours == others => 2 //Leaving this would make this piece hanging
                case (ours, others) if others > ours               => 1 //We are losing on defenders moving would be bad
                case _                                             => 0
              }
            lazy val targetValue = game.board.pieces.get(target).fold(emptySquareValue) {
              case targetPiece if targetPiece.colour == colour => targetPiece.defendValue
              case targetPiece                                 => targetPiece.attackValue
            }
            //println(s"    $target value ${targetValue * attackerDifference} attackers = ${thisAttackers(target).mkString(", ")} otherAttackers = ${otherAttackers(target).mkString(", ")}")
            total + targetValue * attackerDifference
          }) //.tap(total => println(s"  Piece total $total"))
        }
        //.tap(total => println(s"Colour total $total"))
      }
      //println(s"Starting new evaluation")
      pointsByColour(game.initiative) - pointsByColour(game.initiative.other)
    }

  def nextPossibleStates(transpositionTable: Map[State, (Int, Int)]): List[Evaluator] = {
    //TODO use transpositionTable from evaluator
    val (captures, moves) = game.initiativePieces.foldLeft(List.empty[Game], List.empty[Game]) {
      case ((captures, moves), piece) =>
        Position.allSet.foldLeft((captures, moves)) { case ((captures, moves), target) =>
          val newCaptures = game.pieceAt(target).flatMap(targetPiece => game.capture(piece, targetPiece)).toList
          val newMoves = game.moveTo(piece, target).toList
          (captures ++ newCaptures, moves ++ newMoves)
        }
    }
    (game.queenSideCastle.toList ++ game.kingSideCastle.toList ++ captures ++ moves).map(Evaluator(_)).sortBy {
      nextState => //Sort by evaluation of last depth check or static evaluation if no previous data found
        transpositionTable
          .get(nextState.game.state)
          .fold {
            nextState.evaluation
          } { case (evaluation, _) =>
            evaluation
          }
    } //(Ordering[Int].reverse)
  }

  lazy val isTerminal: Boolean = game.ended

  type EvaluationWithPath = (Int, NonEmptyList[Evaluator])
  type TranspositionTable = Map[State, (Int, Int)] //State -> (Evaluation, Depth)
  type TranspositionTableEntry = (State, (Int, Int))

  private def asyncAlphaBetaPath(
    transpositionTable: TranspositionTable,
    maxDepth: Int,
  )(
    root: Evaluator,
    depth: Int,
    alphaWithPath: EvaluationWithPath,
    betaWithPath: EvaluationWithPath,
    path: NonEmptyList[Evaluator],
  ): IO[(TranspositionTable, EvaluationWithPath)] = {
    def getEvaluation(
      transpositionTable: TranspositionTable,
      root: Evaluator,
      alphaWithPath: EvaluationWithPath,
      betaWithPath: EvaluationWithPath,
    ): IO[(TranspositionTable, EvaluationWithPath, Boolean)] = {
      lazy val (alpha, alphaPath) = alphaWithPath
      lazy val (beta, betaPath) = betaWithPath
      transpositionTable
        .get(root.game.state)
        .collect { case (evaluation, `maxDepth`) =>
          //println(s"Transposition hit $evaluation ${FENPrinter((path :+ root).last.game).create}")
          IO.pure((transpositionTable, (evaluation, path :+ root)))
        }
        .getOrElse(
          asyncAlphaBetaPath(
            transpositionTable,
            maxDepth,
          )(
            root,
            depth - 1,
            (-beta, betaPath),
            (-alpha, alphaPath),
            path :+ root,
          ),
        )
        .map { case (transpositionTable, (eval, updatedPath)) =>
          val evaluation = -eval
          /*println(
            List(
              s"D = $depth",
              s"E = ${evaluation.toString.padTo(11, ' ')}",
              s"α = ${alpha.toString.padTo(11, ' ')}",
              s"β = ${beta.toString.padTo(11, ' ')}",
              moves(updatedPath.toList),
              FENPrinter(updatedPath.last.game).create,
            ).mkString(" "),
          )*/

          if (evaluation >= beta) {
            //println("Snip")
            (transpositionTable, betaWithPath, false)
          } else if (alpha >= evaluation) {
            //println("alpha")
            (transpositionTable, alphaWithPath, true)
          } else {
            //println(s"Found new best ${nextState.game.initiative} $evaluation, ${moves(updatedPath)}")
            (transpositionTable, (evaluation, updatedPath), true)
          }
        }
    }

    if (depth <= 0 || root.isTerminal) {
      val evaluation = /*root.colourMultiplier * */ root.evaluation
      val evaluationWithPath = (evaluation, path)
      val updatedTranspositionTable = transpositionTable + ((root.game.state, (evaluation, maxDepth)))

      //val updatedTranspositionEntry = List((root.game.state, (evaluation, maxDepth)))
      return IO((updatedTranspositionTable, evaluationWithPath))

    }

    /*lazy val (alpha, alphaPath) = alphaWithPath
    if (alpha == -checkmateEvaluation && alphaPath.length - 1 <= maxDepth) {
      println(s"Already found better or equal mate, mate in ${alphaPath.length - 1}")
      return IO.pure((transpositionTable, alphaWithPath))
    }*/

    /*for {
      list <- root.nextPossibleStates(transpositionTable).parTraverse { nextState =>
        getEvaluation(transpositionTable, nextState, alphaWithPath, betaWithPath).map {
          case (transpositionTable, evaluationWithPath, _) => (transpositionTable, evaluationWithPath)
        }
      }
      (transpositionTables, evaluations) = list.unzip
      best = evaluations.maxBy(_._1)
    } yield (transpositionTables.flatten.toMap, best)*/

    val states = root.nextPossibleStates(transpositionTable)
    /*println(
      s"Looking for best move for ${root.game.initiative} depth $depth/$maxDepth moves ${states.length} ${moves(
        path.toList,
      )} ${FENPrinter(root.game).create}",
    )*/
    states
      /*.tap { list =>
        list.foreach { evaluator =>
          lazy val (alpha, _) = alphaWithPath
          val evaluation = transpositionTable
            .get(evaluator.game.state)
            .fold {
              evaluator.evaluation
            } { case (evaluation, _) =>
              evaluation
            }
          println(
            List(
              s"E = ${evaluator.evaluation.toString.padTo(11, ' ')}",
              //s"ET = ${evaluation.toString.padTo(11, ' ')}",
              s"C = ${evaluator.game.initiative}",
              s"α = ${alpha.toString.padTo(11, ' ')}",
              moves((path :+ evaluator).toList),
              FENPrinter(evaluator.game).create,
            ).mkString(" "),
          )
        }
      }*/
      .foldLeft(IO.pure((transpositionTable, alphaWithPath, true))) { case (io, nextState) =>
        io.flatMap {
          case continue @ (_, _, false) =>
            IO.pure(continue)
          case (transpositionTable, mate @ (alpha, alphaPath), _) if alpha == -checkmateEvaluation =>
            //println(s"Found a mate in ${alphaPath.length - 1} skipping")
            IO.pure((transpositionTable, mate, false))
          case (transpositionTable, alphaWithPath, _) =>
            getEvaluation(transpositionTable, nextState, alphaWithPath, betaWithPath)
        }
      }
      .map { case (transpositionEntries, evaluationWithPath, _) =>
        (transpositionEntries, evaluationWithPath)
      }
  }

  private def transformResult(
    result: (TranspositionTable, (Int, NonEmptyList[Evaluator])),
  )(
    implicit pf: PrinterFactory[_],
  ): IO[Option[Evaluator]] =
    result match {
      case (_, (_, NonEmptyList(_, Nil))) => IO.pure(None)
      case (transpositionTable, (_, NonEmptyList(root, bestMove :: _))) =>
        val game = bestMove.game
        val printBestMove =
          AlgebraicNotation.findMove(root, bestMove).fold(_ => IO.unit, move => IO.println(s"Best move is $move"))
        for {
          _ <- printBestMove
          _ <- printChanges(game)
        } yield Some(Evaluator(game, transpositionTable))
    }

  def formatTime(start: Long): String = {
    val diff = System.currentTimeMillis() - start
    diff match {
      case millis if millis < 1000        => s"$millis milliseconds"
      case seconds if seconds < 1000 * 60 => s"${seconds / 1000} seconds"
      case minutes                        => s"${minutes / 1000 / 60} minutes"
    }
  }

  def bestMove(maxDuration: FiniteDuration)(implicit pf: PrinterFactory[_]): IO[Option[Evaluator]] = {
    val startPath = NonEmptyList.one(this)
    val max = (Int.MaxValue, startPath)
    val min: (Int, NonEmptyList[Evaluator]) = (-Int.MaxValue, startPath)
    val bestMoveSoFar = Ref.unsafe[IO, Option[(TranspositionTable, (Int, NonEmptyList[Evaluator]))]](None)
    val start = System.currentTimeMillis()
    val result = List
      .range(1, 11)
      .foldLeft[IO[(TranspositionTable, (Int, NonEmptyList[Evaluator]))]](IO.pure((transpositionTable, min))) {
        case (io, depth) =>
          for {
            bestGame @ (transpositionTable, (evaluation, path)) <- io
            bestResult <-
              if (evaluation == -checkmateEvaluation) {
                IO.pure(bestGame)
              } else {
                IO.println(
                  s"Best after ${formatTime(start)} is $evaluation ${moves(path)}",
                ) *>
                  IO.println(s"Running for depth $depth transposition size ${transpositionTable.size}") *>
                  asyncAlphaBetaPath(transpositionTable, depth)(
                    this,
                    depth,
                    min,
                    max,
                    startPath,
                  )
              }
            _ <- bestMoveSoFar.set(Some(bestResult))
            /*if (depth % 2 == 1) {
                IO.println(s"Best so far is $evaluation ${moves(path)}") *>
                  bestMoveSoFar.set(Some(bestResult))
              } else {
                IO.unit
              }*/
          } yield bestResult
      }
      .flatMap(transformResult)
    val cutoff = for {
      _ <- IO.sleep(maxDuration)
      _ <- IO.println("Time limit reached")
      bestSoFar <- bestMoveSoFar.get
      result <- bestSoFar.fold[IO[Option[Evaluator]]](IO.pure(None))(transformResult)
    } yield result

    result.race(cutoff).map(_.merge)
  }

  private def highlights(otherGame: Game) = game.initiativePieces symDiff otherGame.colourPieces(game.initiative)

  private def printChanges(otherGame: Game)(implicit pf: PrinterFactory[_]): IO[Unit] =
    pf(otherGame)
      .withHighlights(highlights(otherGame).map(_.position))
      .withIcons
      .withHeader
      .withFooter(s"${otherGame.initiative} Evaluation = ${Evaluator(otherGame).evaluation}")
      .show

  def changedPieces(otherGame: Game): List[Piece] =
    game.initiativePieces symDiffList otherGame.colourPieces(game.initiative)

  def changedPieces(evaluator: Evaluator): List[Piece] = changedPieces(evaluator.game)

  def changes(evaluator: Evaluator): String =
    AlgebraicNotation.findMove(this, evaluator).fold(e => throw new Exception(e.message), identity)

  private def moves(nel: NonEmptyList[Evaluator]): String = {
    @tailrec def moves(list: List[Evaluator], text: Option[String] = None): String =
      list match {
        case Nil      => ""
        case _ :: Nil => ""
        case one :: two :: Nil =>
          val move = one.changes(two)
          text.fold(move)(text => s"$text, ${one.changes(two)}")
        case one :: two :: tail =>
          val move = one.changes(two)
          moves(two +: tail, Some(text.fold(move)(text => s"$text, ${one.changes(two)}")))
      }
    moves(nel.toList)
  }
}

object Evaluator { outer =>
  val checkmateEvaluation: Int = -Int.MaxValue / 2

  implicit class PieceOps(piece: Piece) {

    lazy val captureValue: Int = piece match {
      case _: King   => 10_000_000
      case _: Queen  => 9000
      case _: Rook   => 5000
      case _: Bishop => 3100
      case _: Knight => 3000
      case _: Pawn   => 1000
    }

    lazy val attackValue: Int = piece match {
      case _: King   => 1000
      case _: Queen  => 450
      case _: Rook   => 250
      case _: Bishop => 155
      case _: Knight => 150
      case _: Pawn   => 50
    }

    lazy val defendValue: Int = piece match {
      case _: King   => 0
      case _: Queen  => 450
      case _: Rook   => 250
      case _: Bishop => 155
      case _: Knight => 15
      case _: Pawn   => 50
    }

    lazy val threat: Int = piece match {
      case _: King   => 1
      case _: Queen  => 1
      case _: Rook   => 5
      case _: Bishop => 7
      case _: Knight => 7
      case _: Pawn   => 9
    }
  }

  lazy val totalBoardPoints: Int = List(
    16 * Pawn(White, A8).captureValue,
    4 * Knight(White, A8).captureValue,
    4 * Bishop(White, A8).captureValue,
    4 * Rook(White, A8).captureValue,
    2 * Queen(White, A8).captureValue,
    2 * King(White, A8).captureValue,
  ).sum

  implicit class SumOps[A](val i: Iterable[A]) extends AnyVal {

    def sumBy[T](f: A => T)(implicit T: Numeric[T]): T = i.foldLeft(T.zero) { case (sum, a) =>
      T.plus(sum, f(a))
    }
  }

  private def distanceWeight(position: Position) =
    1 + 1 / (2 * math.sqrt(math.pow(position.rankInt - 3.5, 2) + math.pow(position.fileInt - 3.5, 2)))

  private lazy val positionWeights: Map[Position, Double] = Position.all.map { position =>
    position -> distanceWeight(position)
  }.toMap

  private lazy val weightedPositions: Map[Int, Map[Position, Double]] = List
    .range(0, 40)
    .map { i =>
      i -> positionWeights.view.mapValues(value => (Math.pow(.95, i) * (value - 1)) + 1).toMap
    }
    .toMap
    .withDefaultValue(Map.empty.withDefaultValue(1))
}
