package chess.pieces

import chess._
import chess.{Board, King, Pawn, Piece}
import org.scalatest.Assertion

abstract class PieceSpec(val pieceType: PieceType) extends ChessSpec with Attacker {
  type PossibleKings = Position => Set[(King, King)]
  type BoardBuilder = (PossibleKings, Piece) => Board

  def boardWithPiece(possibleKings: PossibleKings, piece: Piece): Board = {
    val (attackingKing, defendingKing) = possibleKings(piece.position).head
    Board.withPieces(attackingKing, defendingKing, piece)
  }

  /** A game with the piece this spec tests in the given attackerPosition and a pawn of the opposing colour in the given targetPosition
    * @param attackerPosition Position for the attacking piece
    * @param targetPosition Position for the defending Pawn
    * @return A board with these two pieces
    */
  def singleCaptureBoard(possibleKings: PossibleKings, attackingPiece: Piece, target: Pawn): Board = {
    val (attackingKing, defendingKing) = possibleKings(attackingPiece.position).find {
      case (attackingKing, defendingKing) =>
        (target.captures & Set(attackingKing, defendingKing).map(_.position)).isEmpty
    }.get

    Board.withPieces(
      attackingKing,
      defendingKing,
      attackingPiece,
      target,
    )
  }

  private def boardWithPieceSurrounded(possibleKings: PossibleKings, piece: Piece, pawnsColour: Colour): Board = {
    val (attackingKing, defendingKing) = possibleKings(piece.position).find { case (attackingKing, defendingKing) =>
      (piece.position.allAround & Set(attackingKing, defendingKing).map(_.position)).isEmpty
    }.get
    val pawns = piece.position.allAround.map(Pawn(pawnsColour, _))
    val pieces = List(attackingKing, defendingKing, piece) ++ pawns
    println(attackingKing)
    println(defendingKing)
    println(pieces)
    Board.withPieces(pieces: _*)
  }

  def boardWithPieceSurroundedByEnemies(possibleKings: PossibleKings, piece: Piece): Board =
    boardWithPieceSurrounded(possibleKings, piece, defender)

  def boardWithPieceSurroundedByAllies(possibleKings: PossibleKings, piece: Piece): Board =
    boardWithPieceSurrounded(possibleKings, piece, attacker)

  def possibleMoves(boardBuilder: BoardBuilder, possibleKings: PossibleKings, position: Position): Set[Position] = {
    val piece = pieceType.create(attacker, position)
    val board = boardBuilder(possibleKings, piece)
    println(board)
    Game(attacker, board).moves(piece)
  }

  def checkPossibleMoves(
    boardBuilder: BoardBuilder,
    possibleKings: PossibleKings,
    position: Position,
    possibleMoves: Position => Set[Position],
  ): Assertion =
    this.possibleMoves(boardBuilder, possibleKings, position) should contain only (possibleMoves(position).toList: _*)

  def possibleAttacks(boardBuilder: BoardBuilder, possibleKings: PossibleKings, position: Position): Set[Position] = {
    val piece = pieceType.create(attacker, position)
    val board = boardBuilder(possibleKings, piece)
    Game(attacker, board).threatenedPositions(piece)
  }

  def checkPossibleAttacks(
    boardBuilder: BoardBuilder,
    possibleKings: PossibleKings,
    position: Position,
    possibleAttacks: Position => Set[Position],
  ): Assertion =
    this.possibleAttacks(boardBuilder, possibleKings, position) should contain only (possibleAttacks(
      position,
    ).toList: _*)

  def possibleCaptures(boardBuilder: BoardBuilder, possibleKings: PossibleKings, position: Position): Set[Piece] = {
    val piece = pieceType.create(attacker, position)
    val board = boardBuilder(possibleKings, piece)
    Game(attacker, board).captures(piece)
  }

  def checkSingleCapture(
    possibleKings: PossibleKings,
    attackerPosition: Position,
    targetPosition: Position,
  ): Assertion = {
    val attackingPiece = pieceType.create(attacker, attackerPosition)
    val target: Pawn = Pawn(defender, targetPosition)
    val board = singleCaptureBoard(possibleKings, attackingPiece, target)
    Game(attacker, board).captures(attackingPiece) should contain only target
  }
}
