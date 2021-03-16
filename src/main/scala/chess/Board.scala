package chess

case class Board(
  whites: Map[Position, Piece],
  blacks: Map[Position, Piece],
) {
  lazy val pieces: Map[Position, Piece] = whites ++ blacks

  def colourPieces(colour: Colour): Map[Position, Piece] = colour.perColour(whites)(blacks)

  def owner(position: Position): Option[Colour] = pieces.get(position).map(_.colour)

  def isOccupied(position: Position): Boolean = owner(position).nonEmpty

  def removePieces(remove: Piece*): Board = {
    val (whiteRemovals, blackRemovals) = Board.mapAndPartition(remove.toList)
    copy(
      whites = whites -- whiteRemovals.keys,
      blacks = blacks -- blackRemovals.keys,
    )
  }

  def addPieces(remove: Piece*): Board = {
    val (whiteAdditions, blackAdditions) = Board.mapAndPartition(remove.toList)
    copy(
      whites = whites ++ whiteAdditions,
      blacks = blacks ++ blackAdditions,
    )
  }

  lazy val hasBothKings: Boolean = whites.exists(_._2.isInstanceOf[King]) && blacks.exists(_._2.isInstanceOf[King])
}

object Board {

  def startingPositions(colour: Colour): Map[Position, Piece] = {
    def pawnRank(line: Int): Map[Position, Piece] =
      Position.byRank(line).map(position => position -> Pawn(colour, position)).toMap

    val pieces = List(
      Rook.apply(colour, _: Position),
      Knight.apply(colour, _: Position, hasMoved = true),
      Bishop.apply(colour, _: Position, hasMoved = true),
      Queen.apply(colour, _: Position, hasMoved = true),
      King.apply(colour, _: Position),
      Bishop.apply(colour, _: Position, hasMoved = true),
      Knight.apply(colour, _: Position, hasMoved = true),
      Rook.apply(colour, _: Position),
    )

    def backRank(line: Int): Map[Position, Piece] = Position
      .byRank(line)
      .zip(pieces)
      .map { case (position, positionToPiece) =>
        position -> positionToPiece(position)
      }
      .toMap

    colour match {
      case Black => pawnRank(6) ++ backRank(7)
      case White => pawnRank(1) ++ backRank(0)
    }
  }

  lazy val initial: Board = Board(startingPositions(White), startingPositions(Black))

  private def mapAndPartition(pieces: List[Piece]): (Map[Position, Piece], Map[Position, Piece]) =
    pieces.map(piece => piece.position -> piece).toMap.partition(_._2.colour == White)

  def withPieces(pieces: Piece*): Board =
    (Board.apply _).tupled(mapAndPartition(pieces.toList))
}
