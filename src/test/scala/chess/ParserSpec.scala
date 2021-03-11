package chess

import cats.data.OptionT
import cats.effect.IO
import org.scalatest.OptionValues
import cats.effect.unsafe.implicits.global

class ParserSpec extends ChessSpec with OptionValues {
  "Notation parser" should {
    "parse moves correctly" in {
      val moves = Parser.movesFromPGN(
        "1. e4 c5 2. Nc3 Nc6 3. f4 g6 4. Nf3 Bg7 5. Bb5 Nd4 6. a4 e6 7. e5 a6 8. Bc4 d6" +
          " 9. exd6 Qxd6 10. d3 Ne7 11. Ne4 Qc7 12. c3 Nxf3+ 13. Qxf3 O-O 14. Be3 b6 15. Nxc5 bxc5 16. Qxa8 Nc6 17. " +
          "Bxa6 Bd7 18. Qb7 Qd6 19. Qb5 Nd4 20. Qc4 Nc2+ 21. Kd2 Nxe3 22. Kxe3 Bc6 23. d4 cxd4+ 24. cxd4 Bxg2 25. Rhg1 " +
          "Bd5 26. Qd3 Rb8 27. Bb5 Bh6 28. Rgf1 Rc8 29. a5 Qb4 30. a6 Qxb2 31. Rab1 Qxh2 32. Rf2 Qg3+ 33. Ke2 Qg4+ 34. " +
          "Kf1 Bxf4 35. Bd7 Bc4 36. Qxc4 Qh3+ 37. Kg1 Qg3+ 38. Kf1 Rxc4 39. a7 Qh3+ 40. Kg1 Be3 41. a8$12Q+ Kg7 42. Qg2" +
          " Bxf2+ 43. Qxf2 Rc3 44. Qh2 Qg4+ 45. Kf1 Rf3+ 46. Ke1 Qe4+ 47. Kd2 Qxb1 48. Qe5+ Rf6 49. Kc3 Qc1+ 50. Kd3 " +
          "Qf4 51. Qc5 Qf3+ 52. Kd2 Rf5 53. Qc4 Qg2+ 54. Kc3 Qg3+ 55. Kb4 Qd6+ 56. Kb3 Qxd7 57. Qc3 Qb5+ 58. Kc2 Rf2+ " +
          "59. Kc1 Qf1+ 60. Qe1 Qxe1#",
      )
      val converted = List(
        "e4",
        "c5",
        "Nc3",
        "Nc6",
        "f4",
        "g6",
        "Nf3",
        "Bg7",
        "Bb5",
        "Nd4",
        "a4",
        "e6",
        "e5",
        "a6",
        "Bc4",
        "d6",
        "exd6",
        "Qxd6",
        "d3",
        "Ne7",
        "Ne4",
        "Qc7",
        "c3",
        "Nxf3+",
        "Qxf3",
        "O-O",
        "Be3",
        "b6",
        "Nxc5",
        "bxc5",
        "Qxa8",
        "Nc6",
        "Bxa6",
        "Bd7",
        "Qb7",
        "Qd6",
        "Qb5",
        "Nd4",
        "Qc4",
        "Nc2+",
        "Kd2",
        "Nxe3",
        "Kxe3",
        "Bc6",
        "d4",
        "cxd4+",
        "cxd4",
        "Bxg2",
        "Rhg1",
        "Bd5",
        "Qd3",
        "Rb8",
        "Bb5",
        "Bh6",
        "Rgf1",
        "Rc8",
        "a5",
        "Qb4",
        "a6",
        "Qxb2",
        "Rab1",
        "Qxh2",
        "Rf2",
        "Qg3+",
        "Ke2",
        "Qg4+",
        "Kf1",
        "Bxf4",
        "Bd7",
        "Bc4",
        "Qxc4",
        "Qh3+",
        "Kg1",
        "Qg3+",
        "Kf1",
        "Rxc4",
        "a7",
        "Qh3+",
        "Kg1",
        "Be3",
        "a8Q+",
        "Kg7",
        "Qg2",
        "Bxf2+",
        "Qxf2",
        "Rc3",
        "Qh2",
        "Qg4+",
        "Kf1",
        "Rf3+",
        "Ke1",
        "Qe4+",
        "Kd2",
        "Qxb1",
        "Qe5+",
        "Rf6",
        "Kc3",
        "Qc1+",
        "Kd3",
        "Qf4",
        "Qc5",
        "Qf3+",
        "Kd2",
        "Rf5",
        "Qc4",
        "Qg2+",
        "Kc3",
        "Qg3+",
        "Kb4",
        "Qd6+",
        "Kb3",
        "Qxd7",
        "Qc3",
        "Qb5+",
        "Kc2",
        "Rf2+",
        "Kc1",
        "Qf1+",
        "Qe1",
        "Qxe1#",
      )
      moves shouldEqual converted
    }
    "parse FEN correctly" in {
      Parser.gameFromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").value shouldEqual Game.initial
    }
    "parse FEN correctly with en passant enabled" in {
      Parser
        .gameFromFEN("rnbqkbnr/1p1ppppp/p7/2pP4/8/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 2")
        .get
        .executeMove("dxc6")
        .value
        .unsafeRunSync()
        .isRightOrThrow shouldBe true
    }
  }

}
