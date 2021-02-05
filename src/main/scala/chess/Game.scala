package chess

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.either._

/** Knows the rules of the game
  * Can verify if a move is legal, if a king is in check or if the game has ended either by draw or checkmate
  */
case class Game(
  initiative: Colour, // Who has the right to move
  board: Board,
  whiteCaptures: Set[Piece] = Set.empty,
  blackCaptures: Set[Piece] = Set.empty,
  moveLog: List[String] = Nil,
) {

  def print(highlights: Set[Position]): IO[Unit] = {
    val printer = GamePrinter.withIcons(this).withHighlights(highlights).withHeader.withMoves
    if (isCheckMate) {
      printer.withFooter(s"Checkmate! ${initiative.other} Wins!").print
    } else {
      printer.print
    }
  }

  def pieceAt(position: Position): Either[Error, Piece] =
    board.pieces.get(position).toRight(PieceNotFound(position))

  def colourPieces(colour: Colour): Set[Piece] = colour.perColour(board.whites)(board.blacks).values.toSet

  lazy val initiativePieces: Set[Piece] = colourPieces(initiative)

  /** Lists all positions the piece in the given position threatens.
    */
  def threatenedPositions(piece: Piece): Set[Position] =
    piece.captures.filter(simpleCanAttack(piece, _).isRight)

  /** Lists the pieces of the given colour that can attack the given position */
  def attackers(position: Position, colour: Colour): Set[Piece] =
    colourPieces(colour).filter(simpleCanAttack(_, position).isRight)

  /** Check if the given position is under attack by the given colour */
  def isUnderAttack(position: Position, colour: Colour): Boolean =
    attackers(position, colour).nonEmpty

  private def obstaclesInTheWay(piece: Piece, startingPosition: Position, target: Position): Boolean =
    !piece.jumpsPieces && (startingPosition to target).exists(board.isOccupied)

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
      !board.isOccupied(target),
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

  def simpleCanCapture(piece: Piece, target: Piece): Either[Error, Unit] = for {
    _ <- simpleCanAttack(piece, target.position)
    _ <- Either.cond(
      piece.colour != target.colour,
      (),
      TargetIsAllied(this, piece.position, target.position),
    )
  } yield ()

  def canMoveTo(piece: Piece, target: Position): Either[Error, Game] = for {
    _ <- simpleCanMoveTo(piece, target)
    boardAfterMove = unsafeMoveTo(piece, target).withSwappedInitiative
    _ <- Either.cond(
      !boardAfterMove.kingIsInCheck(initiative),
      (),
      KingWouldEndUpInCheck(this, piece.position, target),
    )
  } yield boardAfterMove

  private def moveTo(piece: Piece, target: Position): Either[Error, Game] =
    canMoveTo(piece, target)

  def canCapture(piece: Piece, target: Piece): Either[Error, Game] =
    for {
      _ <- simpleCanCapture(piece, target)
      _ <- checkInitiative(piece)
      boardAfterCapture = unsafeCapture(piece, target).withSwappedInitiative
      game <- target match {
        case _ if boardAfterCapture.kingIsInCheck(piece.colour) =>
          KingWouldEndUpInCheck(this, piece.position, target.position).asLeft

        case _ => boardAfterCapture.asRight
      }
    } yield game

  private def capture(piece: Piece, target: Piece): Either[Error, Game] =
    canCapture(piece, target)

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
    checkCaptures(piece, (canCapture(piece, _)).andThen(_.toOption))

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
      _ = println(s"rookPosition = $rookPosition")
      _ = println(s"pieceAtRookPosition = $pieceAtRookPosition")
      _ = println(s"board = $board")
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
        !underAttackPositions.exists(isUnderAttack(_, initiative.other)),
        (),
        RouteIsUnderAttack(this, underAttackPositions: _*),
      )
      //No need to check if king ends up in check since the line above already does that
    } yield unsafeMoveTo(king, finalKingPosition).unsafeMoveTo(rook, finalRookPosition).withSwappedInitiative
  }

  private lazy val kingSideCastle: Either[Error, Game] = {
    val rookPosition = initiative.perColour[Position](H1)(H8)
    val emptyPositions = initiative.perColour(List(F1, G1))(List(F8, G8))
    val underAttackPositions = initiative.perColour(List(E1, F1, G1))(List(E8, F8, G8))
    val finalKingPosition = initiative.perColour[Position](G1)(G8)
    val finalRookPosition = initiative.perColour[Position](F1)(F8)
    castle(rookPosition, emptyPositions, underAttackPositions, finalKingPosition, finalRookPosition)
  }

  private lazy val queenSideCastle: Either[Error, Game] = {
    val rookPosition = initiative.perColour[Position](A1)(A8)
    val emptyPositions = initiative.perColour(List(B1, C1, D1))(List(B8, C8, D8))
    val underAttackPositions = initiative.perColour(List(C1, D1, E1))(List(C8, D8, E8))
    val finalKingPosition = initiative.perColour[Position](C1)(C8)
    val finalRookPosition = initiative.perColour[Position](D1)(D8)
    castle(rookPosition, emptyPositions, underAttackPositions, finalKingPosition, finalRookPosition)
  }

  def updateBoard(f: Board => Board): Game = copy(board = f(board))

  lazy val withSwappedInitiative: Game = copy(initiative = initiative.other)

  private def addToLog(move: String): Game = copy(moveLog = moveLog :+ move)

  private def unsafeMoveTo(piece: Piece, target: Position): Game =
    updateBoard(_.removePieces(piece).addPieces(piece.moveToPosition(target)))

  private def unsafeCapture(piece: Piece, target: Piece): Game =
    updateBoard(_.removePieces(piece, target).addPieces(piece.moveToPosition(target.position)))

  private def findKing(colour: Colour): King =
    colourPieces(colour)
      .collectFirst { case king: King => king }
      .getOrElse(throw new Exception(s"King not found ${GamePrinter.withIcons(this).string}()"))

  def kingIsInCheck(colour: Colour): Boolean = isUnderAttack(findKing(colour).position, colour.other)

  private lazy val noPossibleMoves = initiativePieces.forall { piece =>
    moves(piece).isEmpty && captures(piece).isEmpty
  }

  lazy val isDraw: Boolean = ???

  /** Checks if initiative king is in checkmate */
  lazy val isCheckMate: Boolean = kingIsInCheck(initiative) && noPossibleMoves

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

  def executeMove(move: String): EitherT[IO, Error, Game] =
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
    }

  def executeMoves(firstMove: String, moreMoves: String*): EitherT[IO, Error, Game] =
    moreMoves.foldLeft(executeMove(firstMove)) { case (state, move) =>
      state.flatMap(_.executeMove(move))
    }

  def executeMoves(moves: List[String]): EitherT[IO, Error, Game] = executeMoves(moves.head, moves.tail: _*)
  def executePGNMoves(moves: String): EitherT[IO, Error, Game] = executeMoves(Notation.convertFromPGN(moves))

}

object Game {
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
    ): EitherT[IO, Error, Game] = {
      val game = for {
        game <- either
        kingPosition = game.findKing(game.initiative).position
        _ <- game.validate(mustEndInCheck, _.kingIsInCheck(game.initiative), MoveIsNotCheck(game, kingPosition))
        _ <- game.validate(mustEndInMate, _.isCheckMate, MoveIsNotMate(game, kingPosition))
      } yield game.addToLog(move)
      EitherT(game.traverse(game => game.print(highlights.toSet).as(game)))
    }
  }
}
