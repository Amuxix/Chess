package chess

import cats.syntax.option._

object Parser {

  val iconReplacements: Map[String, String] = Map(
    ((c: Colour) => King(c, A1)) -> "K",
    ((c: Colour) => Queen(c, A1)) -> "Q",
    ((c: Colour) => Rook(c, A1)) -> "R",
    ((c: Colour) => Bishop(c, A1)) -> "B",
    ((c: Colour) => Knight(c, A1)) -> "N",
  ).flatMap { case (builder, letter) =>
    List(White, Black).map(builder(_).icon -> letter)
  }

  private def convertIconsToString(string: String) =
    iconReplacements.foldLeft(string) { case (string, replacement) =>
      (string.replaceAll _).tupled(replacement)
    }

  def movesFromPGN(moves: String): List[String] =
    moves
      .replaceAll("\\d+\\. ", "") //removeTurnNumbers
      .replaceAll("\\$12", "")    //Encoded =
      .replaceAll("=", "")        //Non Encoded =
      .split(" ")                 // split into single moves
      .map(convertIconsToString)
      .toList

  def gameFromFEN(fen: String): Option[Game] = {
    val line = "[1-8QKRBNPqkrbnp]+".r
    val allLines = s"((?:$line/){7}$line)".r
    val activeColour = "([wb])".r
    val castlingAvailability = "([KQkq]{1,4}|-)".r
    val enPassant = "([a-h][1-8]|-)".r
    val halfMoveClock = "(\\d{1,2})".r
    val moves = "(\\d+)".r
    val fullFEN = s"$allLines $activeColour $castlingAvailability $enPassant $halfMoveClock $moves".r
    fen match {
      case fullFEN(joinedLines, activeColour, castlingAvailability, enPassant, _, _) =>
        val lines = joinedLines.split("/").toList
        val initiative = activeColour match {
          case "w" => White
          case "b" => Black
        }
        val pieces =
          lines.zip(List.range(7, -1, -1)).foldLeft(Map.empty[Position, Piece]) { case (pieces, (line, rank)) =>
            val (_, finalPieces) = line.split("").foldLeft((0, pieces)) { case ((file, pieces), char) =>
              lazy val position = Position.byPositionTuple(file, rank)
              def createPiece(string: String): Piece = string match {
                case "Q" => Queen(White, position)
                case "K" => King(White, position)
                case "R" =>
                  val hasMoved = (file != 0 || !castlingAvailability.contains("Q")) &&
                    (file != 7 || !castlingAvailability.contains("K"))
                  Rook(White, position, hasMoved)
                case "B" => Bishop(White, position)
                case "N" => Knight(White, position)
                case "P" => Pawn(White, position, hasMoved = rank != 1)
                case "q" => Queen(Black, position)
                case "k" => King(Black, position)
                case "r" =>
                  val hasMoved = (file != 0 || !castlingAvailability.contains("q")) &&
                    (file != 7 || !castlingAvailability.contains("k"))
                  Rook(Black, position, hasMoved)
                case "b" => Bishop(Black, position)
                case "n" => Knight(Black, position)
                case "p" => Pawn(Black, position, hasMoved = rank != 6)
              }
              char.toIntOption.fold {
                (file + 1, pieces + (position -> createPiece(char)))
              } { empty =>
                (file + empty, pieces)
              }
            }
            finalPieces
          }
        val (whites, blacks) = pieces.partition { case (_, piece) =>
          piece.colour == White
        }
        val board = Board(whites, blacks)
        val lastMove = enPassant match {
          case "-" => None
          case position =>
            Position.fromAlgebraicNotation(position).toOption.map { position =>
              val above = (position + (0, 1)).get
              val below = (position + (0, -1)).get
              val piece = Pawn(initiative.other, initiative.perColour(above)(below))
              val target = initiative.perColour(below)(above)
              (piece, target)
            }
        }
        Game(initiative, board, lastMove = lastMove).some
      case _ => None
    }
  }

}
