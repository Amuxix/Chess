package chess

class MoverParserSpec extends ChessSpec with Attacker {
  override lazy val attacker: Colour = White

  val kingsOnlyParser: MoveParser = Game.withPieces(King(attacker, E1), King(defender, E8)).moveParser

  val noPawnsParser: MoveParser = Game.initial
    .updateBoard(board =>
      board.removePieces(board.pieces.collect { case (_, pawn: Pawn) =>
        pawn
      }.toList: _*),
    )
    .moveParser

  "Move parser" should {
    "parse simple moves correctly" should {
      "the move be a White Pawn double move" in {
        val move = SimpleMove(Pawn(White, E2), E4, mustEndInCheck = false, mustEndInMate = false, "e4")
        Game.initial.moveParser.parse("e4").right.value shouldBe move
      }
      "the move be a White Pawn single move" in {
        val move = SimpleMove(Pawn(White, E2), E3, mustEndInCheck = false, mustEndInMate = false, "e3")
        Game.initial.moveParser.parse("e3").right.value shouldBe move
      }
      "the move be a Black Pawn double move" in {
        val move = SimpleMove(Pawn(Black, E7), E5, mustEndInCheck = false, mustEndInMate = false, "e5")
        Game.initial.withSwappedInitiative.moveParser.parse("e5").right.value shouldBe move
      }
      "the move be a Black Pawn single move" in {
        val move = SimpleMove(Pawn(Black, E7), E6, mustEndInCheck = false, mustEndInMate = false, "e6")
        Game.initial.withSwappedInitiative.moveParser.parse("e6").right.value shouldBe move
      }
      "the move be a King Move" in {
        val move = SimpleMove(King(attacker, E1), E2, mustEndInCheck = false, mustEndInMate = false, "Ke2")
        noPawnsParser.parse("Ke2").right.value shouldBe move
      }
      "the move be a Queen Move" in {
        val move = SimpleMove(Queen(attacker, D1), D2, mustEndInCheck = false, mustEndInMate = false, "Qd2")
        noPawnsParser.parse("Qd2").right.value shouldBe move
      }
      "the move be a Rook Move" in {
        val move = SimpleMove(Rook(attacker, A1), A2, mustEndInCheck = false, mustEndInMate = false, "Ra2")
        noPawnsParser.parse("Ra2").right.value shouldBe move
      }
      "the move be a Bishop" in {
        val move = SimpleMove(Bishop(attacker, C1), D2, mustEndInCheck = false, mustEndInMate = false, "Bd2")
        noPawnsParser.parse("Bd2").right.value shouldBe move
      }
      "the move be a Knight" in {
        val move = SimpleMove(Knight(attacker, B1), A3, mustEndInCheck = false, mustEndInMate = false, "Na3")
        noPawnsParser.parse("Na3").right.value shouldBe move
      }
    }
    "parse simple castles correctly" should {
      val castle = Game.initial.updateBoard(board =>
        board.removePieces(board.pieces.collect {
          case (_, queen: Queen)   => queen
          case (_, bishop: Bishop) => bishop
          case (_, knight: Knight) => knight
        }.toList: _*),
      )
      "the castle be White King side" in {
        val move = KingSideCastle(mustEndInCheck = false, mustEndInMate = false, "O-O")
        castle.moveParser.parse("O-O").right.value shouldBe move
      }
      "the castle be White Queen side" in {
        val move = QueenSideCastle(mustEndInCheck = false, mustEndInMate = false, "O-O-O")
        castle.moveParser.parse("O-O-O").right.value shouldBe move
      }
      "the castle be Black King side" in {
        val move = KingSideCastle(mustEndInCheck = false, mustEndInMate = false, "O-O")
        castle.withSwappedInitiative.moveParser.parse("O-O").right.value shouldBe move
      }
      "the castle be Black Queen side" in {
        val move = QueenSideCastle(mustEndInCheck = false, mustEndInMate = false, "O-O-O")
        castle.withSwappedInitiative.moveParser.parse("O-O-O").right.value shouldBe move
      }
    }
    "parse simple captures correctly" should {
      "the capture be a White Pawn capture" ignore {}
      "the capture be a Black Pawn capture" ignore {}
      "the capture be a King capture" ignore {}
      "the capture be a Queen capture" ignore {}
      "the capture be a Rook capture" ignore {}
      "the capture be a Bishop capture" ignore {}
      "the capture be a Knight capture" ignore {}
    }
    "parse simple promotions correctly" should {
      "the promotion be a Queen promotion" ignore {}
      "the promotion be a Rook promotion" ignore {}
      "the promotion be a Bishop promotion" ignore {}
      "the promotion be a Knight promotion" ignore {}
    }
    "parse check moves correctly" should {
      "the check be a White Pawn move" ignore {}
      "the check be a Black Pawn move" ignore {}
      "the check be a King move" ignore {}
      "the check be a Queen move" ignore {}
      "the check be a Rook move" ignore {}
      "the check be a Bishop move" ignore {}
      "the check be a Knight move" ignore {}
    }
    "parse check castles correctly" should {
      "the castle be White King side" ignore {}
      "the castle be White Queen side" ignore {}
      "the castle be Black King side" ignore {}
      "the castle be Black Queen side" ignore {}
    }
    "parse check captures correctly" should {
      "the check be a White Pawn capture" ignore {}
      "the check be a Black Pawn capture" ignore {}
      "the check be a King capture" ignore {}
      "the check be a Queen capture" ignore {}
      "the check be a Rook capture" ignore {}
      "the check be a Bishop capture" ignore {}
      "the check be a Knight capture" ignore {}
    }
    "parse check promotions correctly" should {
      "the check be a Queen promotion" ignore {}
      "the check be a Rook promotion" ignore {}
      "the check be a Bishop promotion" ignore {}
      "the check be a Knight promotion" ignore {}
    }
    "parse mate moves correctly" should {
      "the mate be a White Pawn move" ignore {}
      "the mate be a Black Pawn move" ignore {}
      "the mate be a King move" ignore {}
      "the mate be a Queen move" ignore {}
      "the mate be a Rook move" ignore {}
      "the mate be a Bishop move" ignore {}
      "the mate be a Knight move" ignore {}
    }
    "parse mate castles correctly" should {
      "the castle be White King side" ignore {}
      "the castle be White Queen side" ignore {}
      "the castle be Black King side" ignore {}
      "the castle be Black Queen side" ignore {}
    }
    "parse mate captures correctly" should {
      "the mate be a White Pawn capture" ignore {}
      "the mate be a Black Pawn capture" ignore {}
      "the mate be a King capture" ignore {}
      "the mate be a Queen capture" ignore {}
      "the mate be a Rook capture" ignore {}
      "the mate be a Bishop capture" ignore {}
      "the mate be a Knight capture" ignore {}
    }
    "parse mate promotions correctly" should {
      "the mate be a Queen promotion" ignore {}
      "the mate be a Rook promotion" ignore {}
      "the mate be a Bishop promotion" ignore {}
      "the mate be a Knight promotion" ignore {}
    }
  }
}
