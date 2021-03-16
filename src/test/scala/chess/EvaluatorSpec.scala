package chess

import cats.effect.unsafe.implicits.global
import scala.concurrent.duration._

class EvaluatorSpec extends ChessSpec {
  "Evaluator should" should {
    "Get next move correctly" in {
      val game = Parser.gameFromFEN("rnbqkbnr/ppppp1pp/5p2/8/4P3/2N5/PPPP1PPP/R1BQKBNR b - - 1 1").get
      Evaluator(game).bestMove(10.seconds).attempt.unsafeRunSync().fold(throw _, _.isDefined) shouldBe true
    }
    "Evaluate black checkmated correctly" in {
      val game = Parser.gameFromFEN("Q4bkr/4p1p1/8/5pNp/5B2/8/PPR3PP/2K1q3 w - - 0 22").get
      Evaluator(game).evaluation shouldBe Evaluator.checkmateEvaluation
    }
    "Evaluate white checkmated correctly" in {
      val game = Parser.gameFromFEN("5bkr/6p1/8/3QppNp/5B2/8/PPK3PP/4R3 b - - 1 23").get
      Evaluator(game).evaluation shouldBe -Evaluator.checkmateEvaluation
    }
    "Give same evaluation for two states with piece colours reversed" in {
      val whites = Parser.gameFromFEN("k6Q/8/8/8/8/8/R7/K7 w - - 0 1").get
      val blacks = Parser.gameFromFEN("K6q/8/8/8/8/8/r7/k7 b - - 0 1").get
      Evaluator(whites).evaluation shouldBe Evaluator(blacks).evaluation
    }
    "Swapping initiative on board with checkmate should return same absolute value" in {
      val game = Parser.gameFromFEN("Q4bkr/4p1p1/8/5pNp/5B2/8/PPR3PP/2K1q3 w - - 0 22").get
      Evaluator(game).evaluation shouldBe -Evaluator(game.withSwappedInitiative).evaluation
    }
    "Swapping initiative on board should return same absolute value" in {
      val game = Parser.gameFromFEN("k6Q/8/8/8/8/8/R7/K7 w - - 0 1").get
      Evaluator(game).evaluation shouldNot be(-Evaluator(game.withSwappedInitiative).evaluation)
    }
  }

}
