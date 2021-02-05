package chess

import cats.data.NonEmptyList
import cats.effect.unsafe.IORuntime
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.enablers.Emptiness.emptinessOfGenTraversable

/*class BoardSpec extends AnyWordSpec with Matchers with EitherValues {
  private val onThePlay = White
  private val waiting = onThePlay.other
  s"A Board with $onThePlay to play" should {
    "not allow capturing allies" in {
      Board
        .withPieces(
          (A1, Queen(onThePlay)),
          (A2, v1.Queen(onThePlay)),
        )
        .canCapture(A1, A2)
        .isLeft shouldBe true
    }
    s"not allow $waiting to move" in {
      Board.withPieces((A1, King(waiting))).canMoveTo(A1, A2).isLeft shouldBe true
    }
    s"not allow $waiting to capture" in {
      Board.withPieces((A1, v1.King(waiting)), (A2, v1.King(onThePlay))).canCapture(A1, A2).isLeft shouldBe true
    }
    "detect attackers correctly" should {
      "the attacker be a King" in {
        val attacker: (Position, Piece) = (D3, v1.King(waiting))
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, Pawn(onThePlay)),
            attacker,
          )
          .attackers(D4, waiting) should contain only attacker
      }
      "the attacker be a Queen" in {
        val attacker: (Position, Piece) = (D3, v1.Queen(waiting))
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.Pawn(onThePlay)),
            attacker,
          )
          .attackers(D4, waiting) should contain only attacker
      }
      "the attacker be a Rook" in {
        val attacker: (Position, Piece) = (D3, Rook(waiting))
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.Pawn(onThePlay)),
            attacker,
          )
          .attackers(D4, waiting) should contain only attacker
      }
      "the attacker be a Bishop" in {
        val attacker: (Position, Piece) = (E3, Bishop(waiting))
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.Pawn(onThePlay)),
            attacker,
          )
          .attackers(D4, waiting) should contain only attacker
      }
      "the attacker be a Knight" in {
        val attacker: (Position, Piece) = (F3, Knight(waiting))
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.Pawn(onThePlay)),
            attacker,
          )
          .attackers(D4, waiting) should contain only attacker
      }
      "the attacker be a Pawn" in {
        val attacker: (Position, Piece) = (E5, v1.Pawn(waiting))
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.Pawn(onThePlay)),
            attacker,
          )
          .attackers(D4, waiting) should contain only attacker
      }
      "there be a list of attackers" in {
        val attackers: List[(Position, Piece)] = List(
          (D1, v1.Rook(waiting)),
          (C5, v1.Pawn(waiting)),
          (E5, v1.Pawn(waiting)),
          (G1, v1.Bishop(waiting)),
          (A1, v1.Queen(waiting)),
          (B5, v1.Knight(waiting)),
          (C6, v1.Knight(waiting)),
          (E6, v1.Knight(waiting)),
          (F5, v1.Knight(waiting)),
          (F3, v1.Knight(waiting)),
          (E2, v1.Knight(waiting)),
          (C2, v1.Knight(waiting)),
          (B3, v1.Knight(waiting)),
        )
        Board
          .withPieces(
            (D4, v1.King(onThePlay)),
            (H8, v1.King(waiting)),
            (D1, v1.Rook(waiting)),
            (C5, v1.Pawn(waiting)),
            (E5, v1.Pawn(waiting)),
            (G1, v1.Bishop(waiting)),
            (A1, v1.Queen(waiting)),
            (B5, v1.Knight(waiting)),
            (C6, v1.Knight(waiting)),
            (E6, v1.Knight(waiting)),
            (F5, v1.Knight(waiting)),
            (F3, v1.Knight(waiting)),
            (E2, v1.Knight(waiting)),
            (C2, v1.Knight(waiting)),
            (B3, v1.Knight(waiting)),
          )
          .attackers(D4, waiting) should contain only (attackers: _*)
      }
    }
    "detect check correctly" should {
      "the attacker be a Queen" in {
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.King(onThePlay)),
            (D3, v1.Queen(waiting)),
          )
          .kingIsInCheck(onThePlay) shouldBe true
      }
      "the attacker be a Rook" in {
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.King(onThePlay)),
            (D3, v1.Rook(waiting)),
          )
          .kingIsInCheck(onThePlay) shouldBe true
      }
      "the attacker be a far away Rook" in {
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (A8, v1.King(onThePlay)),
            (A1, v1.Rook(waiting)),
          )
          .kingIsInCheck(onThePlay) shouldBe true
      }
      "the attacker be a Bishop" in {
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.King(onThePlay)),
            (E3, v1.Bishop(waiting)),
          )
          .kingIsInCheck(onThePlay) shouldBe true
      }
      "the attacker be a far away Bishop" in {
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (A8, v1.King(onThePlay)),
            (H1, v1.Bishop(waiting)),
          )
          .kingIsInCheck(onThePlay) shouldBe true
      }
      "the attacker be a Knight" in {
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.King(onThePlay)),
            (F3, v1.Knight(waiting)),
          )
          .kingIsInCheck(onThePlay) shouldBe true
      }
      "the attacker be a Pawn" in {
        Board
          .withPieces(
            (H8, v1.King(waiting)),
            (D4, v1.King(onThePlay)),
            (E5, v1.Pawn(waiting)),
          )
          .kingIsInCheck(onThePlay) shouldBe true
      }
      "there be a list of attackers" in {
        Board
          .withPieces(
            (D4, v1.King(onThePlay)),
            (H8, v1.King(waiting)),
            (D1, v1.Rook(waiting)),
            (C5, v1.Pawn(waiting)),
            (E5, v1.Pawn(waiting)),
            (G1, v1.Bishop(waiting)),
            (A1, v1.Queen(waiting)),
            (B5, v1.Knight(waiting)),
            (C6, v1.Knight(waiting)),
            (E6, v1.Knight(waiting)),
            (F5, v1.Knight(waiting)),
            (F3, v1.Knight(waiting)),
            (E2, v1.Knight(waiting)),
            (C2, v1.Knight(waiting)),
            (B3, v1.Knight(waiting)),
          )
          .kingIsInCheck(onThePlay) shouldBe true
      }
    }
    "detect a checkmate" in {
      Board
        .withPieces(
          (A8, v1.King(onThePlay)),
          (H8, v1.King(waiting)),
          (A1, v1.Rook(waiting)),
          (B1, v1.Rook(waiting)),
        )
        .isCheckMate shouldBe true
    }
    "not detect a checkmate when king can capture attacking piece" in {
      Board
        .withPieces(
          (A8, v1.King(onThePlay)),
          (H8, v1.King(waiting)),
          (B7, v1.Queen(waiting)),
        )
        .isCheckMate shouldBe false
    }
    "not allow a king to capture a protected piece" in {
      Board
        .withPieces(
          (A8, v1.King(onThePlay)),
          (H8, v1.King(waiting)),
          (B7, v1.Queen(waiting)),
          (H1, v1.Bishop(waiting)),
        )
        .canCapture(A8, B7)
        .isLeft shouldBe true
    }
    "detect a checkmate when an attacking piece is in capture range" in {
      Board
        .withPieces(
          (A8, v1.King(onThePlay)),
          (H8, v1.King(waiting)),
          (B7, v1.Queen(waiting)),
          (H1, v1.Bishop(waiting)),
        )
        .isCheckMate shouldBe true
    }
    "detect a draw when white king has no legal moves but not in check" in {
      Board
        .withPieces(
          (A8, v1.King(onThePlay)),
          (H8, v1.King(waiting)),
          (H7, v1.Queen(waiting)),
          (B1, v1.Queen(waiting)),
        )
        .isDraw shouldBe true
    }

    "detect a dead position draw" should {
      "the pieces on board be a king versus king" in {
        Board
          .withPieces(
            (A8, v1.King(onThePlay)),
            (H8, v1.King(waiting)),
          )
          .isDraw shouldBe true
      }
      "the pieces on board a king and a bishop versus a king" in {
        Board
          .withPieces(
            (A8, v1.King(onThePlay)),
            (B8, v1.Bishop(onThePlay)),
            (H8, v1.King(waiting)),
          )
          .isDraw shouldBe true
      }
      "the pieces on board a king and a knight versus a king" in {
        Board
          .withPieces(
            (A8, v1.King(onThePlay)),
            (H8, v1.King(waiting)),
          )
          .isDraw shouldBe true
      }
      "the pieces on board a king and a bishop versus a king and a bishop if the bishops are on the same colour" in {
        Board
          .withPieces(
            (A8, v1.King(onThePlay)),
            (B8, v1.Bishop(onThePlay)),
            (H8, v1.King(waiting)),
            (G7, v1.Bishop(waiting)),
          )
          .isDraw shouldBe true
      }
    }
    "not detect a draw when a the only pieces on the board are king and bishop versus king and bishop if the bishops are different colours" in {
      Board
        .withPieces(
          (A8, v1.King(onThePlay)),
          (B8, v1.Bishop(onThePlay)),
          (H8, v1.King(waiting)),
          (G8, v1.Bishop(waiting)),
        )
        .isDraw shouldBe false
    }
    "when no pieces are in play no squares should be under attack" in {
      (for {
        colour <- List(onThePlay, waiting)
        position <- Position.all
        attackers = Board.empty.attackers(position, colour).map(_._1)
      } yield attackers).flatten shouldBe empty
    }
    "be able to play full game" in {
      Board.initial.executeMoves(
        "b3",
        "e5",
        "Bb2",
        "Nc6",
        "e3",
        "d5",
        "Bb5",
        "Ne7",
        "Bxe5",
        "a6",
        "Bxc6+",
        "Nxc6",
        "Bg3",
        "h5",
        "h3",
        "d4",
        "Nf3",
        "h4",
        "Bh2",
        "Bf5",
        "O-O",
        "Be7",
        "d3",
        "Qd7",
        "e4",
        "Be6",
        "Nbd2",
        "g5",
        "Ne5",
        "Nxe5",
        "Bxe5",
        "f6",
        "Bh2",
        "g4",
        "hxg4",
        "Bxg4",
        "f3",
        "Be6",
        "f4",
        "Bg4",
        "Qe1",
        "h3",
        "g3",
        "O-O-O",
        "Nc4",
        "Kb8",
        "f5",
        "Rhg8",
        "Kh1",
        "Bc5",
        "Qf2",
        "Qg7",
        "Qf4",
        "Rde8",
        "a3",
        "b5",
        "Na5",
        "Bd6",
        "Qf2",
        "Bh5",
        "Nc6+",
        "Kb7",
        "Nxd4",
        "Bxg3",
        "Bxg3",
        "Qxg3",
        "Qxg3",
        "Rxg3",
        "Rf2",
        "Reg8",
        "Kh2",
        "c5",
        "Ne6",
        "Kb6",
        "Nf4",
        "Bf7",
        "Raf1",
        "a5",
        "Rf3",
        "R3g4",
        "Rxh3",
        "b4",
        "Rh6",
        "bxa3",
        "Rxf6+",
        "Kb5",
        "Rxf7",
        "a2",
        "Ng6",
        "Kb4",
        "Ra7",
        "Rg5",
        "Ra1",
        "Kc3",
        "Rxa2",
        "a4",
        "R2xa4",
        "Kd2",
        "Rc4",
        "Ke3",
        "c3",
        "Kf2",
        "Ra2+",
        "Kf3",
        "e5",
        "Rg7",
        "Rh4",
        "Ke3",
        "Rh3",
      ) shouldBe 'right
    }
    "play Karpov vs Kasparov (1985)" in {
      val moves = NonEmptyList.fromListUnsafe(
        Notation.parseMoves(
          "1.e4 c5 2.Nf3 e6 3.d4 cxd4 4.Nxd4 Nc6 5.Nb5 d6 6.c4 Nf6 7.N1c3 a6 8.Na3 d5 9.cxd5 exd5 " +
            "10.exd5 Nb4 11.Be2 Bc5 12.O-O O-O 13.Bf3 Bf5 14.Bg5 Re8 15.Qd2 b5 16.Rad1 Nd3 17.Nab1 h6 18.Bh4 b4 " +
            "19.Na4 Bd6 20.Bg3 Rc8 21.b3 g5 22.Bxd6 Qxd6 23.g3 Nd7 24.Bg2 Qf6 25.a3 a5 26.axb4 axb4 27.Qa2 Bg6 " +
            "28.d6 g4 29.Qd2 Kg7 30.f3 Qxd6 31.fxg4 Qd4+ 32.Kh1 Nf6 33.Rf4 Ne4 34.Qxd3 Nf2+ 35.Rxf2 Bxd3 36.Rfd2 Qe3 " +
            "37.Rxd3 Rc1 38.Nb2 Qf2 39.Nd2 Rxd1+ 40.Nxd1 Re1+",
        ),
      )
      val board = Board.initial.executeMoves(moves)
      board.isRight shouldBe true
      import cats.implicits._
      board.traverse_(_.print).unsafeRunSync()(IORuntime.global)
    }
  }
}*/
