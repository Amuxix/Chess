package chess

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.either._
import cats.syntax.functor._
import chess.printers.GamePrinter.PrinterFactory

/** Knows the rules of the game
  * Can verify if a move is legal, if a king is in check or if the game has ended either by draw or checkmate
  */
case class Game(
  state: State,
  whiteCaptures: Set[Piece],
  blackCaptures: Set[Piece],
  moveLog: List[String],
) {
  lazy val initiative: Colour = state.initiative
  lazy val board: Board = state.board
  lazy val lastMove: Option[(Piece, Position)] = state.lastMove

  lazy val fullMoves: Int = moveLog.size / 2

  def show(highlights: Set[Position])(implicit pf: PrinterFactory[_]): IO[Unit] = {
    val printer = pf(this).withHighlights(highlights).withIcons.withHeader.withMoves
    if (isCheckMate) {
      printer.withFooter(s"Checkmate! ${initiative.other} Wins!").show
    } else {
      printer.show
    }
  }

  def pieceAt(position: Position): Either[Error, Piece] =
    board.pieces.get(position).toRight(PieceNotFound(position))

  def colourPieces(colour: Colour): Set[Piece] = colour.perColour(board.whites)(board.blacks).values.toSet

  lazy val initiativePieces: Set[Piece] = colourPieces(initiative)

  /** Lists the pieces of the given colour that can attack the given position */
  private def simpleAttackers(position: Position, colour: Colour): Set[Piece] =
    colourPieces(colour).filter(simpleCanAttack(_, position).isRight)

  /** Check if the given position is under attack by the given colour */
  private def simpleIsUnderAttack(position: Position, colour: Colour): Boolean =
    simpleAttackers(position, colour).nonEmpty

  /** Lists all positions the piece in the given position threatens.
    */
  def threatenedPositions(piece: Piece): Set[Position] =
    piece.captures.filter(canAttack(piece, _).isRight)

  /** Lists the pieces of the given colour that can attack the given position */
  def attackers(position: Position, colour: Colour): Set[Piece] =
    colourPieces(colour).filter(canAttack(_, position).isRight)

  /** Lists the pieces of the given colour that can move to the given position */
  def movers(position: Position, colour: Colour): Set[Piece] =
    colourPieces(colour).filter(canMoveTo(_, position).isRight)

  /** Check if the given position is under attack by the given colour */
  def isUnderAttack(position: Position, colour: Colour): Boolean =
    attackers(position, colour).nonEmpty

  def isOccupied(position: Position): Boolean = board.isOccupied(position)

  private def obstaclesInTheWay(piece: Piece, startingPosition: Position, target: Position): Boolean =
    !piece.jumpsPieces && (startingPosition to target).exists(isOccupied)

  private def checkForObstacles(piece: Piece, target: Position): Either[Error, Unit] =
    Either.cond(!obstaclesInTheWay(piece, piece.position, target), (), ObstacleInTheWay(this, piece.position, target))

  private def checkInitiative(piece: Piece): Either[Error, Unit] =
    Either.cond(piece.colour == initiative, (), WrongInitiative)

  def simpleCanMoveTo(piece: Piece, target: Position): Either[Error, Unit] = for {
    _ <- checkForObstacles(piece, target)
    _ <- Either.cond(
      piece.moves.contains(target),
      (),
      PieceCantMoveThatWay(this, piece.position, target),
    )
    _ <- Either.cond(
      !isOccupied(target),
      (),
      TargetOccupied(target, this, piece.position, target),
    )
  } yield ()

  def simpleCanAttack(piece: Piece, target: Position): Either[Error, Unit] = for {
    _ <- checkForObstacles(piece, target)
    _ <- Either.cond(
      piece.captures.contains(target),
      (),
      PieceCantMoveThatWay(this, piece.position, target),
    )
  } yield ()

  def simpleCanCapture(piece: Piece, target: Piece): Either[Error, Position] = for {
    _ <- Either.cond(
      piece.colour != target.colour,
      (),
      TargetIsAllied(this, piece.position, target.position),
    )
    position <- enPassantCanCapture(piece, target).orElse(simpleCanAttack(piece, target.position).as(target.position))
  } yield position

  def enPassantCanCapture(piece: Piece, target: Piece): Either[Error, Position] = for {
    _ <- Either.cond(
      piece.isInstanceOf[Pawn],
      (),
      InvalidCapturingPiece,
    )
    (lastMovedPawnBeforeMove, lastMoveTargetPosition) <- lastMove
      .collect { case lastMove @ (_: Pawn, _) =>
        lastMove
      }
      .toRight(InvalidLastMovedPieceForEnPassant)
    lastMovedPawn <- pieceAt(lastMoveTargetPosition)
    _ <- Either.cond(
      lastMovedPawn == target,
      (),
      InvalidTargetForEnPassant,
    )
    _ <- Either.cond(
      math.abs(lastMovedPawnBeforeMove.position.rankInt - lastMoveTargetPosition.rankInt) == 2,
      (),
      NoPreviousDoubleStepMoveEnPassant(this, piece.position, target.position),
    )
    _ <- Either.cond(
      piece.colour.perColour(piece.position.rankInt == 4)(piece.position.rankInt == 3),
      (),
      WrongRankForEnPassant(this, piece.position, target.position),
    )
    _ <- Either.cond(
      math.abs(piece.position.fileInt - target.position.fileInt) == 1,
      (),
      WrongFileForEnPassant(this, piece.position, target.position),
    )
    position <- lastMovedPawnBeforeMove.colour
      .perColour(lastMovedPawnBeforeMove.position + (0, 1))(lastMovedPawnBeforeMove.position + (0, -1))
      .toRight(EnPassantNotPossible)
  } yield position

  def canMoveTo(piece: Piece, target: Position): Either[Error, Game] = for {
    _ <- simpleCanMoveTo(piece, target)
    boardAfterMove = unsafeMoveTo(piece, target).withSwappedInitiative
    _ <- Either.cond(
      !boardAfterMove.kingIsInCheck(initiative),
      (),
      KingWouldEndUpInCheck(this, piece.position, target),
    )
  } yield boardAfterMove

  def moveTo(piece: Piece, target: Position): Either[Error, Game] =
    canMoveTo(piece, target)

  /** Checks if piece could take target piece, regardless of the target piece colour
    */
  def canAttack(piece: Piece, target: Position): Either[Error, Unit] =
    simpleCanAttack(piece, target).flatMap { _ =>
      pieceAt(target).toOption match {
        case Some(king: King) if king.colour == piece.colour =>
          ().asRight
        case Some(targetPiece) if unsafeCapture(piece, targetPiece, target).kingIsInCheck(piece.colour) =>
          KingWouldEndUpInCheck(this, piece.position, target).asLeft
        case None if unsafeMoveTo(piece, target).kingIsInCheck(piece.colour) =>
          KingWouldEndUpInCheck(this, piece.position, target).asLeft
        case _ =>
          ().asRight
      }
    }

  def canCapture(piece: Piece, targetPiece: Piece): Either[Error, Game] =
    for {
      target <- simpleCanCapture(piece, targetPiece)
      _ <- checkInitiative(piece)
      boardAfterCapture = unsafeCapture(piece, targetPiece, target).withSwappedInitiative
      game <-
        if (boardAfterCapture.kingIsInCheck(piece.colour)) {
          KingWouldEndUpInCheck(this, piece.position, targetPiece.position).asLeft
        } else {
          boardAfterCapture.asRight
        }
    } yield game

  def capture(piece: Piece, targetPiece: Piece): Either[Error, Game] =
    canCapture(piece, targetPiece)

  /** List of moves the given piece can make only doing the simple checks */
  def simpleMoves(piece: Piece): Set[Position] = piece.moves.filter(simpleCanMoveTo(piece, _).isRight)

  def moves(piece: Piece): Set[Position] = piece.moves.filter(canMoveTo(piece, _).isRight)

  private def checkCaptures(piece: Piece, check: Piece => Option[_]): Set[Piece] = for {
    position <- piece.captures
    target <- pieceAt(position).toOption
    _ <- check(target)
  } yield target

  /** List of captures the given piece can make only doing the simple checks */
  def simpleCaptures(piece: Piece): Set[Piece] =
    checkCaptures(piece, (simpleCanCapture(piece, _)).andThen(_.toOption))

  def captures(piece: Piece): Set[Piece] =
    checkCaptures(piece, ((targetPiece: Piece) => canCapture(piece, targetPiece)).andThen(_.toOption))

  private def castle(
    rookPosition: Position,
    emptyPositions: List[Position],
    underAttackPositions: List[Position],
    finalKingPosition: Position,
    finalRookPosition: Position,
  ): Either[Error, Game] = {
    val king = findKing(initiative)
    for {
      _ <- Either.cond(!king.hasMoved, (), KingAlreadyMoved(king))
      pieceAtRookPosition <- pieceAt(rookPosition)
      rook <- pieceAtRookPosition match {
        case rook: Rook if !rook.hasMoved => rook.asRight
        case _                            => RookAlreadyMoved(pieceAtRookPosition).asLeft
      }
      _ <- Either.cond(
        emptyPositions.flatMap(board.pieces.get).isEmpty,
        (),
        ObstacleInTheWay(this, emptyPositions: _*),
      )
      _ <- Either.cond(
        !underAttackPositions.exists(simpleIsUnderAttack(_, initiative.other)),
        (),
        RouteIsUnderAttack(this, underAttackPositions: _*),
      )
      //No need to check if king ends up in check since the line above already does that
    } yield unsafeMoveTo(king, finalKingPosition).unsafeMoveTo(rook, finalRookPosition).withSwappedInitiative
  }

  lazy val kingSideCastle: Either[Error, Game] = {
    val rookPosition = initiative.perColour[Position](H1)(H8)
    val emptyPositions = initiative.perColour(List(F1, G1))(List(F8, G8))
    val underAttackPositions = initiative.perColour(List(E1, F1, G1))(List(E8, F8, G8))
    val finalKingPosition = initiative.perColour[Position](G1)(G8)
    val finalRookPosition = initiative.perColour[Position](F1)(F8)
    castle(rookPosition, emptyPositions, underAttackPositions, finalKingPosition, finalRookPosition)
  }

  lazy val queenSideCastle: Either[Error, Game] = {
    val rookPosition = initiative.perColour[Position](A1)(A8)
    val emptyPositions = initiative.perColour(List(B1, C1, D1))(List(B8, C8, D8))
    val underAttackPositions = initiative.perColour(List(C1, D1, E1))(List(C8, D8, E8))
    val finalKingPosition = initiative.perColour[Position](C1)(C8)
    val finalRookPosition = initiative.perColour[Position](D1)(D8)
    castle(rookPosition, emptyPositions, underAttackPositions, finalKingPosition, finalRookPosition)
  }

  def updateBoard(f: Board => Board): Game = copy(state = state.updateBoard(f))

  lazy val withSwappedInitiative: Game = copy(state = state.withSwappedInitiative)

  private def addToLog(move: String): Game = copy(moveLog = moveLog :+ move)

  private def unsafeMoveTo(piece: Piece, target: Position): Game = {
    val state = this.state.copy(
      board = board.removePieces(piece).addPieces(piece.moveToPosition(target)),
      lastMove = Some((piece, target)),
    )
    copy(state = state)
  }

  private def unsafeCapture(piece: Piece, targetPiece: Piece, target: Position): Game = {
    val state = this.state.copy(
      board = board.removePieces(piece, targetPiece).addPieces(piece.moveToPosition(target)),
      lastMove = Some((piece, target)),
    )
    copy(state = state)
  }

  def findKing(colour: Colour): King =
    colourPieces(colour).collectFirst { case king: King => king }.getOrElse(throw new Exception(s"King not found"))

  def kingIsInCheck(colour: Colour): Boolean = simpleIsUnderAttack(findKing(colour).position, colour.other)

  private lazy val noPossibleMovesOrCaptures: Boolean =
    initiativePieces.flatMap(piece => moves(piece) ++ captures(piece)).isEmpty

  private lazy val isStalemate = !kingIsInCheck(initiative) && noPossibleMovesOrCaptures

  private lazy val insufficientMaterial = {
    val pieces = board.pieces.values
    pieces.size match {
      case 2 => true // King vs King
      case 3 =>
        pieces.exists {
          case _: Bishop => true //King and Bishop vs King
          case _: Knight => true //King and Knight vs King
          case _         => false
        }
      case 4 =>
        board.blacks.exists {
          case (blackBishopPosition, _: Bishop) =>
            board.blacks.exists {
              case (whiteBishopPosition, _: Bishop) =>
                //King and Bishop vs King and Bishop on same colour
                blackBishopPosition.squareColour == whiteBishopPosition.squareColour
              case _ => false
            }
          case _ => false
        }
      case _ => false
    }
  }

  lazy val isDraw: Boolean = isStalemate || insufficientMaterial

  /** Checks if initiative king is in checkmate */
  lazy val isCheckMate: Boolean = kingIsInCheck(initiative) && noPossibleMovesOrCaptures

  lazy val ended: Boolean = noPossibleMovesOrCaptures

  private def validate(condition: Boolean, validation: Game => Boolean, error: => Error): Either[Error, Unit] =
    if (condition) {
      Either.cond(
        validation(this),
        (),
        error,
      )
    } else {
      ().asRight
    }

  lazy val moveParser = new MoveParser(this)

  def executeMove(move: String)(implicit pf: PrinterFactory[_]): EitherT[IO, Error, Game] =
    EitherT.fromEither[IO](moveParser.parse(move)).flatMap {
      case SimpleMove(piece, target, mustEndInCheck, mustEndInMate, move) =>
        moveTo(piece, target).finalizeMove(mustEndInCheck, mustEndInMate, move)(piece.position, target)

      case Capture(piece, target, mustEndInCheck, mustEndInMate, move) =>
        capture(piece, target).finalizeMove(mustEndInCheck, mustEndInMate, move)(piece.position, target.position)

      case KingSideCastle(mustEndInCheck, mustEndInMate, move) =>
        val highlights = initiative.perColour(List(E1, G1))(List(E8, G1))
        kingSideCastle.finalizeMove(mustEndInCheck, mustEndInMate, move)(highlights: _*)

      case QueenSideCastle(mustEndInCheck, mustEndInMate, move) =>
        val highlights = initiative.perColour(List(E1, C1))(List(E8, C1))
        queenSideCastle.finalizeMove(mustEndInCheck, mustEndInMate, move)(highlights: _*)

      case Promotion(pawn, target, promotion, mustEndInCheck, mustEndInMate, move) =>
        moveTo(promotion.create(pawn.colour, pawn.position), target)
          .finalizeMove(mustEndInCheck, mustEndInMate, move)(pawn.position, target)

      case CapturePromotion(pawn, target, promotion, mustEndInCheck, mustEndInMate, move) =>
        capture(promotion.create(pawn.colour, pawn.position), target)
          .finalizeMove(mustEndInCheck, mustEndInMate, move)(pawn.position, target.position)

      case EnPassantCapture(piece, targetPosition, target, mustEndInCheck, mustEndInMate, move) =>
        capture(piece, target).finalizeMove(mustEndInCheck, mustEndInMate, move)(piece.position, targetPosition)
    }

  def executeMoves(firstMove: String, moreMoves: String*)(implicit pf: PrinterFactory[_]): EitherT[IO, Error, Game] =
    moreMoves.foldLeft(executeMove(firstMove)) { case (state, move) =>
      state.flatMap(_.executeMove(move))
    }

  def executeMoves(moves: List[String])(implicit pf: PrinterFactory[_]): EitherT[IO, Error, Game] =
    executeMoves(moves.head, moves.tail: _*)

  def executePGNMoves(moves: String)(implicit pf: PrinterFactory[_]): EitherT[IO, Error, Game] = executeMoves(
    Parser.movesFromPGN(moves),
  )
}

object Game {

  def apply(
    initiative: Colour,
    board: Board,
    whiteCaptures: Set[Piece] = Set.empty,
    blackCaptures: Set[Piece] = Set.empty,
    moveLog: List[String] = Nil,
    lastMove: Option[(Piece, Position)] = None,
  ): Game = Game(State(initiative, board, lastMove), whiteCaptures, blackCaptures, moveLog)

  lazy val initial: Game = Game(White, Board.initial)

  def withPieces(pieces: Piece*): Game =
    Game(White, Board.withPieces(pieces: _*))

  implicit class Finalizer(val either: Either[Error, Game]) extends AnyVal {

    def finalizeMove(
      mustEndInCheck: Boolean,
      mustEndInMate: Boolean,
      move: String,
    )(
      highlights: Position*,
    )(
      implicit pf: PrinterFactory[_],
    ): EitherT[IO, Error, Game] = {
      val game = for {
        game <- either
        kingPosition = game.findKing(game.initiative).position
        _ <- game.validate(mustEndInCheck, _.kingIsInCheck(game.initiative), MoveIsNotCheck(game, kingPosition))
        _ <- game.validate(mustEndInMate, _.isCheckMate, MoveIsNotMate(game, kingPosition))
      } yield game.addToLog(move)
      EitherT(game.traverse(game => game.show(highlights.toSet).as(game)))
    }
  }
}
