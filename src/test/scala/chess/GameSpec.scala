package chess

import cats.effect.unsafe.implicits.global

class GameSpec extends ChessSpec {
  "Game" should {
    "play game with en passant" in {
      Game.initial
        .executeMoves(
          "e4",
          "a5",
          "e5",
          "d5",
          "exd6",
        )
        .value
        .unsafeRunSync()
        .isRightOrThrow shouldBe true
    }
    "should fail when trying invalid en passant" in {
      Parser
        .gameFromFEN("8/8/3p4/KPp4r/1R3p1k/4P3/6P1/8 w - c6 0 1")
        .get
        .executeMove("bxc6")
        .value
        .unsafeRunSync()
        .isLeftOrThrow shouldBe true
    }
    "be able to play full game" in {
      Game.initial
        .executeMoves(
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
        )
        .value
        .unsafeRunSync()
        .isRightOrThrow shouldBe true
    }
    "play full game" in {
      val moves = "1. e4 c5 2. Nc3 Nc6 3. f4 g6 4. Nf3 Bg7 5. Bb5 Nd4 6. a4 e6 7. e5 a6 8. Bc4 d6" +
        " 9. exd6 Qxd6 10. d3 Ne7 11. Ne4 Qc7 12. c3 Nxf3+ 13. Qxf3 O-O 14. Be3 b6 15. Nxc5 bxc5 16. Qxa8 Nc6 17. " +
        "Bxa6 Bd7 18. Qb7 Qd6 19. Qb5 Nd4 20. Qc4 Nc2+ 21. Kd2 Nxe3 22. Kxe3 Bc6 23. d4 cxd4+ 24. cxd4 Bxg2 25. Rhg1 " +
        "Bd5 26. Qd3 Rb8 27. Bb5 Bh6 28. Rgf1 Rc8 29. a5 Qb4 30. a6 Qxb2 31. Rab1 Qxh2 32. Rf2 Qg3+ 33. Ke2 Qg4+ 34. " +
        "Kf1 Bxf4 35. Bd7 Bc4 36. Qxc4 Qh3+ 37. Kg1 Qg3+ 38. Kf1 Rxc4 39. a7 Qh3+ 40. Kg1 Be3 41. a8$12Q+ Kg7 42. Qg2" +
        " Bxf2+ 43. Qxf2 Rc3 44. Qh2 Qg4+ 45. Kf1 Rf3+ 46. Ke1 Qe4+ 47. Kd2 Qxb1 48. Qe5+ Rf6 49. Kc3 Qc1+ 50. Kd3 " +
        "Qf4 51. Qc5 Qf3+ 52. Kd2 Rf5 53. Qc4 Qg2+ 54. Kc3 Qg3+ 55. Kb4 Qd6+ 56. Kb3 Qxd7 57. Qc3 Qb5+ 58. Kc2 Rf2+ " +
        "59. Kc1 Qf1+ 60. Qe1 Qxe1#"

      Game.initial.executePGNMoves(moves).value.unsafeRunSync().isRightOrThrow shouldBe true
    }
  }
}
