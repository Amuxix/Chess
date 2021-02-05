package chess

import org.scalatest.enablers.Emptiness.emptinessOfGenTraversable

class PositionSpec extends ChessSpec {
  "To function" should {
    "return correct files" in {
      A1 to A8 should contain only (A2, A3, A4, A5, A6, A7)
      A8 to A1 should contain only (A2, A3, A4, A5, A6, A7)

      A2 to A7 should contain only (A3, A4, A5, A6)
      A7 to A2 should contain only (A3, A4, A5, A6)
    }
    "return correct ranks" in {
      A1 to H1 should contain only (B1, C1, D1, E1, F1, G1)
      H1 to A1 should contain only (B1, C1, D1, E1, F1, G1)

      B1 to G1 should contain only (C1, D1, E1, F1)
      G1 to B1 should contain only (C1, D1, E1, F1)
    }
    "return correct diagonals" in {
      //A1 to H8 should contain only (B2, C3, D4, E5, F6, G7)
      //H8 to A1 should contain only (B2, C3, D4, E5, F6, G7)

      A8 to H1 should contain only (B7, C6, D5, E4, F3, G2)
      //H1 to A8 should contain only (B7, C6, D5, E4, F3, G2)

      //B1 to H7 should contain only (C2, D3, E4, F5, G6)
      //H7 to B1 should contain only (C2, D3, E4, F5, G6)

      //A7 to G1 should contain only (B6, C5, D4, E3, F2)
      //G1 to A7 should contain only (B6, C5, D4, E3, F2)
    }
    "return empty when pieces are adjacent" in {
      A1 to A2 shouldBe empty
      A2 to A1 shouldBe empty

      A1 to B1 shouldBe empty
      B1 to A1 shouldBe empty

      A1 to B2 shouldBe empty
      B2 to A1 shouldBe empty

      A2 to B1 shouldBe empty
      B1 to A2 shouldBe empty
    }

    "return empty when positions not in a line" in {
      A1 to B3 shouldBe empty
      C6 to E2 shouldBe empty
    }
  }
}
