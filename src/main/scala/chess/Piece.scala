package chess

import cats.syntax.option._

sealed abstract class Piece extends Named {
  def colour: Colour
  def position: Position
  def hasMoved: Boolean
  def pieceType: PieceType
  val jumpsPieces: Boolean = pieceType.jumpsPieces
  def moveToPosition(position: Position): Piece

  lazy val vectorMoves: Set[(Int, Int)] =
    pieceType.moves(colour) ++ (if (!hasMoved) pieceType.firstPlayMoves(colour) else Nil)
  lazy val vectorCaptures: Set[(Int, Int)] = pieceType.captures(colour)

  lazy val moves: Set[Position] =
    for {
      vector <- vectorMoves
      target <- position + vector
    } yield target

  lazy val captures: Set[Position] =
    for {
      vector <- vectorCaptures
      target <- position + vector
    } yield target
}

object Piece {
  def unapply(piece: Piece): Option[(Colour, Position, Boolean)] = Some((piece.colour, piece.position, piece.hasMoved))

  /*implicit def ordering(positionOrdering: Ordering[Position]): Ordering[Piece] =
    (x: Piece, y: Piece) => positionOrdering.compare(x.position, y.position)*/
}

sealed trait PieceType extends Named {
  val jumpsPieces: Boolean = false
  def moves(colour: Colour): Set[(Int, Int)]
  def firstPlayMoves(colour: Colour): Set[(Int, Int)]
  def captures(colour: Colour): Set[(Int, Int)]

  def create(colour: Colour, position: Position): Piece
}

object PieceType {

  def fromString(string: String): Option[PieceType] = string match {
    case "K" => King.some
    case "Q" => Queen.some
    case "R" => Rook.some
    case "B" => Bishop.some
    case "N" => Knight.some
    case _   => None
  }

}

sealed trait SameCaptures extends PieceType {
  def moves: Set[(Int, Int)]

  override def moves(colour: Colour): Set[(Int, Int)] = moves

  override def firstPlayMoves(colour: Colour): Set[(Int, Int)] = Set.empty

  override def captures(colour: Colour): Set[(Int, Int)] = moves
}

object King extends SameCaptures {

  override lazy val moves: Set[(Int, Int)] =
    for {
      file <- Set.range(-1, 2)
      rank <- Set.range(-1, 2)
      if file != 0 || rank != 0
    } yield (file, rank)

  override def create(colour: Colour, position: Position): Piece = King(colour, position, hasMoved = true)
}

case class King(colour: Colour, position: Position, hasMoved: Boolean = false) extends Piece {
  override def pieceType: PieceType = King

  override def moveToPosition(position: Position): King = copy(position = position, hasMoved = true)
}

object Queen extends SameCaptures {

  override lazy val moves: Set[(Int, Int)] = Bishop.moves ++ Rook.moves

  override def create(colour: Colour, position: Position): Piece = Queen(colour, position, hasMoved = true)
}

case class Queen(colour: Colour, position: Position, hasMoved: Boolean = false) extends Piece {
  override def pieceType: PieceType = Queen

  override def moveToPosition(position: Position): Queen = copy(position = position, hasMoved = true)
}

object Rook extends SameCaptures {

  override lazy val moves: Set[(Int, Int)] =
    for {
      file <- Set.range(-7, 8)
      rank <- Set.range(-7, 8)
      if file != 0 || rank != 0
      if rank == 0 || file == 0
    } yield (file, rank)

  override def create(colour: Colour, position: Position): Piece = Rook(colour, position, hasMoved = true)
}

case class Rook(colour: Colour, position: Position, hasMoved: Boolean = false) extends Piece {
  override def pieceType: PieceType = Rook

  override def moveToPosition(position: Position): Rook = copy(position = position, hasMoved = true)
}

object Bishop extends SameCaptures {

  override lazy val moves: Set[(Int, Int)] =
    for {
      file <- Set.range(-7, 8)
      rank <- Set.range(-7, 8)
      if file != 0 || rank != 0
      if math.abs(rank) == math.abs(file)
    } yield (file, rank)

  override def create(colour: Colour, position: Position): Piece = Bishop(colour, position, hasMoved = true)
}

case class Bishop(colour: Colour, position: Position, hasMoved: Boolean = false) extends Piece {
  override def pieceType: PieceType = Bishop

  override def moveToPosition(position: Position): Bishop = copy(position = position, hasMoved = true)
}

object Knight extends SameCaptures {
  override val jumpsPieces: Boolean = true

  override lazy val moves: Set[(Int, Int)] =
    for {
      file <- Set.range(-2, 3)
      rank <- Set.range(-2, 3)
      if math.abs(file) + math.abs(rank) == 3
    } yield (file, rank)

  override def create(colour: Colour, position: Position): Piece = Knight(colour, position, hasMoved = true)
}

case class Knight(colour: Colour, position: Position, hasMoved: Boolean = false) extends Piece {

  override def pieceType: PieceType = Knight

  override def moveToPosition(position: Position): Knight = copy(position = position, hasMoved = true)
}

object Pawn extends PieceType {

  override def moves(colour: Colour): Set[(Int, Int)] = colour.perColour(Set((0, 1)))(Set((0, -1)))
  override def firstPlayMoves(colour: Colour): Set[(Int, Int)] = colour.perColour(Set((0, 2)))(Set((0, -2)))

  override def captures(colour: Colour): Set[(Int, Int)] =
    colour.perColour(Set((1, 1), (-1, 1)))(Set((1, -1), (-1, -1)))

  override def create(colour: Colour, position: Position): Piece = Pawn(colour, position, hasMoved = true)
}

case class Pawn(colour: Colour, position: Position, hasMoved: Boolean = false) extends Piece {
  override def pieceType: PieceType = Pawn

  override def moveToPosition(position: Position): Pawn = copy(position = position, hasMoved = true)
}
