package chess

import cats.effect.IO
import chess.GamePrinter._

case class GamePrinter(
  game: Game,
  lines: List[String],
  hasBoardHeader: Boolean,
) {

  private lazy val pieces: Map[Position, Piece] = game.board.whites ++ game.board.blacks

  private def iconFor(position: Position): String = {
    val icon = pieces.getOrElse(position, position).icon
    s" $icon "
  }

  private def matrixPositionOf(position: Position): (Int, Int) = {
    val rank = (7 - position.rankInt) * 2 + 1
    val file = 4 * position.fileInt + 2
    (file, rank)
  }

  private lazy val allIcons = Position.all.map(iconFor)

  private lazy val boardWithoutIcons = lines.mkString("\n").split("(?<=[┃│]) {3}(?=[┃│])").toList

  private lazy val boardMatrix: List[List[String]] = lines.map(_.split("").toList)

  private def borderReplacements(position: Position): Map[(Int, Int), String => String] = offsetsToReplacements.map {
    case (offset, replacement) =>
      (matrixPositionOf(position) + offset) -> replacement
  }.toMap

  private def matrixWithHighlightedPosition(boardMatrix: List[List[String]], position: Position): List[List[String]] = {
    val replacements: Map[(Int, Int), String => String] = borderReplacements(position).withDefault(_ => identity)
    val matrix = for {
      y <- List.range(0, boardMatrix.length)
      x <- List.range(0, boardMatrix(y).length)
      icon = boardMatrix(y)(x)
      replacer = replacements((x, y))
      replacedIcon = replacer(icon)
    } yield replacedIcon
    matrix.grouped(boardMatrix.head.length).toList
  }

  private lazy val moveLogLines: List[String] = {
    val spaces = " " * spaceBetweenWhiteAndBlackMoves

    def withMove(move: Int, white: String, black: Option[String]): String = {
      val moveStr = String.format("%3s", move + 1) //f"${move + 1}%3s"

      val blackStr = black.fold("")(b => s"$spaces${b.padTo(movePadding, ' ')}")
      s"$moveStr. ${white.padTo(movePadding, ' ')}$blackStr"
    }
    val moveLines = game.moveLog
      .grouped(2)
      .zipWithIndex
      .map {
        case (List(white, black), move) => withMove(move, white, Some(black))
        case (List(white), move)        => withMove(move, white, None)
      }
      .toList
      .grouped(boardHeight - 1)
      .toList
      .foldLeft(List.empty[String]) { case (lines, extraColumn) =>
        lines.zipAll(extraColumn, spaces, "").map { case (firsColumn, secondColumn) =>
          s"$firsColumn$spaces$secondColumn"
        }
      }
    val headerLength = moveLines.headOption.fold(2 * movePadding + spaceBetweenWhiteAndBlackMoves)(_.length)
    val header = createHeader("Move Log", headerLength)
    header +: moveLines
  }

  private def withLines(lines: List[String]): GamePrinter = copy(lines = lines)

  def withHighlights(highlightedPositions: Set[Position]): GamePrinter = {
    //TODO Handle case where this has header already
    val lines = highlightedPositions
      .foldLeft(boardMatrix) { case (matrix, position) =>
        matrixWithHighlightedPosition(matrix, position)
      }
      .map(_.mkString)
    withLines(lines)
  }

  lazy val withIcons: GamePrinter = withLines(boardWithoutIcons.intercalate(allIcons).mkString.split("\n").toList)

  lazy val withHeader: GamePrinter = {
    if (hasBoardHeader) {
      val nonBoardHeader = lines.head.slice(boardLength + 1, lines.head.length)
      val headers = createHeader(s"${game.initiative} to move", boardLength) + nonBoardHeader
      withLines(headers +: lines.tail)
    } else {
      copy(lines = createHeader(s"${game.initiative} to move", boardLength) +: lines, hasBoardHeader = true)
    }
  }

  def withFooter(footer: String): GamePrinter =
    withLines(lines :+ createHeader(footer, lines.head.length))

  lazy val withMoves: GamePrinter = {
    val space = " " * spaceBetweenBoardAndLog
    lazy val fillMissingBoardSideLines = " " * boardLength
    val linesWIthHeader = if (hasBoardHeader) lines else " " * boardLength +: lines //Create empty board header
    val updatedLines =
      linesWIthHeader.zipAll(moveLogLines, fillMissingBoardSideLines, "").map { case (boardLine, logLine) =>
        s"$boardLine$space$logLine"
      }
    copy(lines = updatedLines, hasBoardHeader = true)
  }

  lazy val string: String = lines.map(_.stripTrailing).mkString("\n", "\n", "")
  lazy val print: IO[Unit] = IO.println(string)
}

object GamePrinter {
  private val spaceBetweenWhiteAndBlackMoves = 2
  private val spaceBetweenBoardAndLog = 8
  private val movePadding = 6

  private val emptyBoard = List(
    "┏━━━┯━━━┯━━━┯━━━┯━━━┯━━━┯━━━┯━━━┓  ",
    "┃   │   │   │   │   │   │   │   ┃ 8",
    "┠───┼───┼───┼───┼───┼───┼───┼───┨  ",
    "┃   │   │   │   │   │   │   │   ┃ 7",
    "┠───┼───┼───┼───┼───┼───┼───┼───┨  ",
    "┃   │   │   │   │   │   │   │   ┃ 6",
    "┠───┼───┼───┼───┼───┼───┼───┼───┨  ",
    "┃   │   │   │   │   │   │   │   ┃ 5",
    "┠───┼───┼───┼───┼───┼───┼───┼───┨  ",
    "┃   │   │   │   │   │   │   │   ┃ 4",
    "┠───┼───┼───┼───┼───┼───┼───┼───┨  ",
    "┃   │   │   │   │   │   │   │   ┃ 3",
    "┠───┼───┼───┼───┼───┼───┼───┼───┨  ",
    "┃   │   │   │   │   │   │   │   ┃ 2",
    "┠───┼───┼───┼───┼───┼───┼───┼───┨  ",
    "┃   │   │   │   │   │   │   │   ┃ 1",
    "┗━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┛  ",
    "  a   b   c   d   e   f   g   h    ",
  )

  lazy val boardLength: Int = emptyBoard.head.length
  lazy val boardHeight: Int = emptyBoard.length

  def empty(game: Game): GamePrinter = GamePrinter(game, emptyBoard, hasBoardHeader = false)
  def withIcons(game: Game): GamePrinter = GamePrinter(game, emptyBoard, hasBoardHeader = false).withIcons

  private lazy val topLeftCornerReplacements: Map[String, String] = Map(
    "┼" -> "╆",
    "╅" -> "╈",
    "╄" -> "╊",
    "╃" -> "╋",
    //"╆" -> "╆", //Redundant
    "╇" -> "╋",
    //"╊" -> "╊", //Redundant
    //"╈" -> "╈", //Redundant
    "╉" -> "╋",
    "┯" -> "┳",
    "┠" -> "┣",
  )

  private lazy val topRightCornerReplacements: Map[String, String] = Map(
    "┼" -> "╅",
    //"╅" -> "╅", //Redundant
    "╄" -> "╋",
    "╃" -> "╉",
    "╆" -> "╈",
    "╇" -> "╋",
    "╊" -> "╋",
    //"╈" -> "╈", //Redundant
    //"╉" -> "╉", //Redundant
    "┯" -> "┳",
    "┨" -> "┫",
  )

  private lazy val bottomLeftCornerReplacements: Map[String, String] = Map(
    "┼" -> "╄",
    "╅" -> "╉",
    //"╄" -> "",//Redundant
    "╃" -> "╇",
    "╆" -> "╊",
    //"╇" -> "",//Redundant
    //"╊" -> "",//Redundant
    "╈" -> "╋",
    "╉" -> "╋",
    "┷" -> "┻",
    "┠" -> "┣",
  )

  private lazy val bottomRightCornerReplacements: Map[String, String] = Map(
    "┼" -> "╃",
    "╅" -> "╉",
    "╄" -> "╇",
    //"╃" -> "",//Redundant
    "╆" -> "╋",
    //"╇" -> "",//Redundant
    "╊" -> "╋",
    "╈" -> "╋",
    //"╉" -> "",//Redundant
    "┷" -> "┻",
    "┨" -> "┫",
  )

  private lazy val horizontalReplacements: Map[String, String] = Map(
    "─" -> "━",
  )

  private lazy val verticalReplacements: Map[String, String] = Map(
    "│" -> "┃",
  )

  private lazy val offsetsToReplacements: List[((Int, Int), String => String)] = List(
    (-2, -1) -> topLeftCornerReplacements,
    (-1, -1) -> horizontalReplacements,
    (0, -1) -> horizontalReplacements,
    (1, -1) -> horizontalReplacements,
    (2, -1) -> topRightCornerReplacements,
    (-2, 0) -> verticalReplacements,
    (2, 0) -> verticalReplacements,
    (-2, 1) -> bottomLeftCornerReplacements,
    (-1, 1) -> horizontalReplacements,
    (0, 1) -> horizontalReplacements,
    (1, 1) -> horizontalReplacements,
    (2, 1) -> bottomRightCornerReplacements,
  ).map { case (offset, map) =>
    offset -> map.withDefault(identity)
  }

  private def createHeader(header: String, lineLength: Int): String = {
    val lengthDifference = lineLength - header.length
    if (lengthDifference > 0) {
      val space = " " * (lengthDifference / 2)
      s"$space$header$space"
    } else {
      header
    }
  }

  implicit class ListOps[T](val list: List[T]) extends AnyVal {

    /** Joins two lists by adding elements to the final list alternating, when one list runs out only use the other list
      */
    def intercalate(other: List[T]): List[T] =
      list.map(List(_)).zipAll(other.map(List(_)), Nil, Nil).flatMap(Function.tupled(_ ++ _))
  }

  implicit class TupleOps(val t: (Int, Int)) extends AnyVal {
    def +(o: (Int, Int)): (Int, Int) = (t._1 + o._1, t._2 + o._2)
  }
}
