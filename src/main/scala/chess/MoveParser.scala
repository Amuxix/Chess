package chess

import cats.syntax.either._
import chess.MoveParser._

/** Knows how to parse moves from algebraic notations
  */
class MoveParser(game: Game) {
  private lazy val moveHelpers = new ParserHelper(game)

  private def fromRegexMatch(
    helpers: ParserHelper,
    piece: String,
    file: String,
    rank: String,
    capture: String,
    target: String,
    promotion: String,
    mustEndInCheck: Boolean,
    mustEndInMate: Boolean,
    move: String,
  ): Either[Error, Move] =
    Position.fromAlgebraicNotation(target).flatMap { target =>
      val originPieceType: PieceType = Option(piece).flatMap(PieceType.fromString).getOrElse(Pawn)
      lazy val originFile: Option[String] = Option(file)
      lazy val originRank: Option[String] = Option(rank)
      val isCapture = capture != null
      val promotionPiece: Option[PieceType] = Option(promotion).flatMap(PieceType.fromString)

      lazy val noValidPieceToMove = NoValidPieceToMove(move, game, target)

      def pieceToMove(possiblePieces: Set[Piece]): Either[Error, Piece] =
        helpers.pieceToMove(originFile, originRank, possiblePieces, noValidPieceToMove)

      lazy val pawnToPromote: Either[Error, Pawn] =
        for {
          piece <- pieceToMove(helpers.possibleMovers(originPieceType, target))
          pawn <- piece match {
            case pawn: Pawn => pawn.asRight
            case _          => noValidPieceToMove.asLeft
          }
        } yield pawn

      (promotionPiece, isCapture) match {
        case (Some(promotion), false) =>
          pawnToPromote.map(Promotion(_, target, promotion, mustEndInCheck, mustEndInMate, move))

        case (Some(promotion), true) =>
          for {
            pawn <- pawnToPromote
            targetPiece <- game.pieceAt(target)
          } yield CapturePromotion(pawn, targetPiece, promotion, mustEndInCheck, mustEndInMate, move)

        case (_, true) =>
          for {
            targetPiece <- game.pieceAt(target)
            piece <- pieceToMove(helpers.possibleCaptors(originPieceType, targetPiece))
          } yield Capture(piece, targetPiece, mustEndInCheck, mustEndInMate, move)

        case (_, false) =>
          pieceToMove(helpers.possibleMovers(originPieceType, target)).map { piece =>
            SimpleMove(piece, target, mustEndInCheck, mustEndInMate, move)
          }

        case _ => IllegalMove.asLeft
      }
    }

  def parse(move: String): Either[Error, Move] =
    move match {
      case castleRegex(queenSide, check, mate) if queenSide != null =>
        QueenSideCastle(check != null, mate != null, move).asRight
      case castleRegex(_, check, mate) =>
        KingSideCastle(check != null, mate != null, move).asRight
      case fullMoveRegex(piece, file, rank, capture, target, promotion, check, mate) =>
        fromRegexMatch(moveHelpers, piece, file, rank, capture, target, promotion, check != null, mate != null, move)
      case _ => AlgebraicNotationParseError(move).asLeft //Invalid move
    }

}

object MoveParser {
  private val checkSuffixes = "(\\+)?(#)?".r
  private val fullMoveRegex = s"([KQRBN])?([a-h])?([1-8])?(x)?([a-h][1-8])=?([KQRBN])?$checkSuffixes".r
  private val castleRegex = s"O-O(-O)?$checkSuffixes".r
}

private class ParserHelper(game: Game) {

  private def filterInitiativePieces(filter: Piece => Boolean): Set[Piece] =
    game.initiativePieces.filter(filter)

  private def matchByPieceType(piece: PieceType, possibleMovers: Set[Piece]): Set[Piece] =
    piece match {
      case Pawn   => possibleMovers.collect { case piece: Pawn => piece }
      case Knight => possibleMovers.collect { case piece: Knight => piece }
      case Bishop => possibleMovers.collect { case piece: Bishop => piece }
      case Rook   => possibleMovers.collect { case piece: Rook => piece }
      case Queen  => possibleMovers.collect { case piece: Queen => piece }
      case King   => possibleMovers.collect { case piece: King => piece }
    }

  def possibleMovers(piece: PieceType, target: Position): Set[Piece] =
    matchByPieceType(piece, filterInitiativePieces(game.canMoveTo(_, target).isRight))

  def possibleCaptors(piece: PieceType, target: Piece): Set[Piece] =
    matchByPieceType(piece, filterInitiativePieces(game.canCapture(_, target).isRight))

  def pieceToMove(
    originFile: Option[String],
    originRank: Option[String],
    possiblePieces: Set[Piece],
    error: => NoValidPieceToMove,
  ): Either[Error, Piece] = {
    lazy val origin: Either[Error, Position] =
      for {
        file <- originFile.toRight(MissingFile)
        rank <- originRank.toRight(MissingRank)
        position <- Position.fromAlgebraicNotation(file + rank)
      } yield position
    Option
      .when(possiblePieces.size == 1)(possiblePieces.head)
      .orElse(
        origin.toOption.flatMap { origin =>
          possiblePieces.collectFirst { case piece if piece.position == origin => piece }
        },
      )
      .orElse {
        originFile.flatMap { originFile =>
          val fileMovers = possiblePieces.collect {
            case piece if piece.position.file.equalsIgnoreCase(originFile) => piece
          }
          Option.when(fileMovers.size == 1)(fileMovers.head)
        }
      }
      .orElse {
        originRank.flatMap { originRank =>
          val rankMovers = possiblePieces.collect {
            case piece if piece.position.rank == originRank => piece
          }
          Option.when(rankMovers.size == 1)(rankMovers.head)
        }
      }
      .toRight {
        println(originFile)
        println(originRank)
        println(possiblePieces)
        error
      }
  }
}
