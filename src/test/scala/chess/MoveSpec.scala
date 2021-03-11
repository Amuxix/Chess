package chess

import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/*class MoveSpec extends AnyWordSpec with Matchers with EitherValues {
  "Move" should {
    "validate a castle is performed correctly" should {
      "it be king side" in {
        val board = Board
          .withPieces(
            (A8, King(Black)),p
            (E1, King(White)),
            (H1, Rook(White)),
          )
          .executeMoves("O-O")
        board shouldBe 'right
        board.map { board =>
          board.whites.get(G1).exists(_.isInstanceOf[King]) shouldBe true
          board.whites.get(F1).exists(_.isInstanceOf[Rook]) shouldBe true
        }
      }
      "it be queen side" in {
        val board = Board
          .withPieces(
            (A8, King(Black)),
            (E1, King(White)),
            (A1, Rook(White)),
          )
          .executeMoves("O-O-O")
        board shouldBe 'right
        board.map { board =>
          board.whites.get(C1).exists(_.isInstanceOf[King]) shouldBe true
          board.whites.get(D1).exists(_.isInstanceOf[Rook]) shouldBe true
        }
      }
    }
    "validate a move is a check" should {
      "the move be really a check" in {
        Board
          .withPieces(
            (A8, King(Black)),
            (H8, King(White)),
            (B1, Rook(White)),
          )
          .executeMoves("Ra1+") shouldBe 'right
      }
      "a king side castle be really a check" in {
        Board
          .withPieces(
            (F8, King(Black)),
            (E1, King(White)),
            (H1, Rook(White)),
          )
          .executeMoves("O-O+") shouldBe 'right
      }
      "a queen side castle be really a check" in {
        Board
          .withPieces(
            (D8, King(Black)),
            (E1, King(White)),
            (A1, Rook(White)),
          )
          .executeMoves("O-O-O+") shouldBe 'right
      }
      "the move not be a check" in {
        Board
          .withPieces(
            (A8, King(Black)),
            (H8, King(White)),
            (B1, Rook(White)),
          )
          .executeMoves("Rb2+")
          .left
          .value shouldBe MoveIsNotCheck
      }
      "a king side castle not be a check" in {
        Board
          .withPieces(
            (A8, King(Black)),
            (E1, King(White)),
            (H1, Rook(White)),
          )
          .executeMoves("O-O+")
          .left
          .value shouldBe MoveIsNotCheck
      }
      "a queen side castle not be a check" in {
        Board
          .withPieces(
            (A8, King(Black)),
            (E1, King(White)),
            (A1, Rook(White)),
          )
          .executeMoves("O-O-O+")
          .left
          .value shouldBe MoveIsNotCheck
      }
    }

    "validate a move is a checkmate" should {
      "the move be really a checkmate" in {
        Board
          .withPieces(
            (A8, King(Black)),
            (H8, King(White)),
            (B1, Rook(White)),
            (B2, Queen(White)),
          )
          .executeMoves("Qb7#") shouldBe 'right
      }
      "a king side castle be really a checkmate" in {
        Board
          .withPieces(
            (F8, King(Black)),
            (E2, Queen(White)),
            (G2, Queen(White)),
            (E1, King(White)),
            (H1, Rook(White)),
          )
          .executeMoves("O-O#") shouldBe 'right
      }
      "a queen side castle be really a checkmate" in {
        Board
          .withPieces(
            (D8, King(Black)),
            (E2, Queen(White)),
            (C2, Queen(White)),
            (E1, King(White)),
            (A1, Rook(White)),
          )
          .executeMoves("O-O-O#") shouldBe 'right
      }
      "the move not be a checkmate" in {
        Board
          .withPieces(
            (A8, King(Black)),
            (H8, King(White)),
            (B1, Rook(White)),
          )
          .executeMoves("Rb2#")
          .left
          .value shouldBe MoveIsNotMate
      }
      "a king side castle not be a checkmate" in {
        Board
          .withPieces(
            (A8, King(Black)),
            (E1, King(White)),
            (H1, Rook(White)),
          )
          .executeMoves("O-O#")
          .left
          .value shouldBe MoveIsNotMate
      }
      "a queen side castle not be a checkmate" in {
        Board
          .withPieces(
            (A8, King(Black)),
            (E1, King(White)),
            (A1, Rook(White)),
          )
          .executeMoves("O-O-O#")
          .left
          .value shouldBe MoveIsNotMate
      }
    }
  }
}*/
