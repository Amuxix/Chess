package chess.pieces

import chess._
import org.scalatest.enablers.Emptiness.emptinessOfGenTraversable

abstract class NonDirectionalPieceSpec(pieceType: PieceType) extends PieceSpec(pieceType) {

  def vectors: List[(Int, Int)]

  def moves(position: Position): Set[Position]

  def attacksWhileSurrounded(position: Position): Set[Position]

  def possibleKings(position: Position): Set[(King, King)] = {
    val possibleMoves = moves(position)
    val nonMoves = Position.all.filterNot(possibleMoves.contains).toSet - position
    nonMoves.flatMap { attackingKingPosition =>
      val attackingKing = King(attacker, attackingKingPosition)
      (nonMoves - attackingKingPosition).collect {
        case position if !attackingKing.captures.contains(position) =>
          attackingKing -> King(defender, position)
      }
    }
  }

  private def checkPossibleCaptures(position: Position) =
    moves(position).map(checkSingleCapture(possibleKings, position, _))

  s"A $attacker ${pieceType.className}" should {
    "have correct vectors" in {
      pieceType.moves(attacker) should contain only (vectors: _*)
    }
    for {
      position <- Position.all
      whenText = s" when in $position"
      jumpChecks =
        if (pieceType.jumpsPieces) {
          List(
            (
              s"have normal possible moves if surrounded and piece can jump $whenText",
              (position: Position) =>
                checkPossibleMoves(boardWithPieceSurroundedByAllies, possibleKings, position, moves),
            ),
            (
              s"have normal possible attacks if surrounded and piece can jump $whenText",
              (position: Position) =>
                checkPossibleAttacks(boardWithPieceSurroundedByAllies, possibleKings, position, moves),
            ),
          )
        } else {
          List(
            (
              s"have no possible moves if surrounded and piece can't jump $whenText",
              (position: Position) =>
                possibleMoves(boardWithPieceSurroundedByEnemies, possibleKings, position) shouldBe empty,
            ),
            (
              s"have limited possible attacks if surrounded and piece can't jump $whenText",
              (position: Position) =>
                checkPossibleAttacks(boardWithPieceSurroundedByAllies, possibleKings, position, attacksWhileSurrounded),
            ),
          )
        }
      (test, checker) <- List(
        (
          s"have correct possible moves $whenText",
          (position: Position) => checkPossibleMoves(boardWithPiece, possibleKings, position, moves),
        ),
        (
          s"have correct possible attacks $whenText",
          (position: Position) => checkPossibleAttacks(boardWithPiece, possibleKings, position, moves),
        ),
      ) ++ jumpChecks
    } yield test in {
      checker(position)
    }
    "have same capture moves as possible moves" should {
      Position.all.foreach { position =>
        s"it be on $position" in {
          checkPossibleCaptures(position)
        }
      }
    }
  }
}
