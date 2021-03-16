package chess

import chess.Position.byPositionTuple

sealed abstract class Position(val fileInt: Int, val rankInt: Int) extends Named {
  lazy val algebraicNotation: String = className.toLowerCase
  //lazy val algebraicNotation: String = Position.position2AlgebraicNotation(this)
  lazy val Array(file, rank) = algebraicNotation.split("")
  lazy val squareColour: Colour = if (fileInt % 2 == rankInt % 2) White else Black

  def +(t: (Int, Int)): Option[Position] = t match {
    case (file, rank) => byPositionTuple.get(this.fileInt + file, this.rankInt + rank)
  }

  def to(position: Position): List[Position] = {
    def aToB(a: Int, b: Int) =
      a - b match {
        case x if x >= -1 && x <= 1 => List.empty
        case x if x > 1             => List.range(b + 1, a)
        case x if x < -1            => List.range(b - 1, a, -1)
      }

    if (position.fileInt == fileInt) {
      aToB(position.rankInt, rankInt).map(byPositionTuple(fileInt, _))
    } else if (position.rankInt == rankInt) {
      aToB(position.fileInt, fileInt).map(byPositionTuple(_, rankInt))
    } else if (math.abs(position.fileInt - fileInt) == math.abs(position.rankInt - rankInt)) {
      aToB(position.fileInt, fileInt).zip(aToB(position.rankInt, rankInt)).map(byPositionTuple)
    } else {
      List.empty
    }
  }

  private def rangeExcluding(x: Int): Set[Int] = Set.range(0, x) ++ Set.range(x + 1, 9)

  lazy val sameFile: Set[Position] = rangeExcluding(fileInt).flatMap(byPositionTuple.get(_, rankInt))

  lazy val sameRank: Set[Position] = rangeExcluding(rankInt).flatMap(byPositionTuple.get(fileInt, _))

  lazy val rookMoves: Set[Position] = sameFile ++ sameRank

  lazy val diagonals: Set[Position] =
    for {
      file <- Set.range(-7, 8)
      rank <- Set.range(-7, 8)
      if file != 0 || rank != 0
      if math.abs(rank) == math.abs(file)
      diagonal <- this + (file, rank)
    } yield diagonal

  lazy val queenMoves: Set[Position] = rookMoves ++ diagonals

  lazy val knightMoves: Set[Position] =
    for {
      file <- Set.range(-2, 3)
      rank <- Set.range(-2, 3)
      if math.abs(file) + math.abs(rank) == 3
      knightJump <- this + (file, rank)
    } yield knightJump

  lazy val diagonalsAround: Set[Position] = Set((-1, -1), (1, -1), (-1, 1), (1, 1)).flatMap(this + _)
  lazy val adjacent: Set[Position] = Set((1, 0), (-1, 0), (0, 1), (0, -1)).flatMap(this + _)

  lazy val allAround: Set[Position] = diagonalsAround ++ adjacent
}

object Position {
  val all: List[Position] = List(
    //format: off
    A8, B8, C8, D8, E8, F8, G8, H8,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A1, B1, C1, D1, E1, F1, G1, H1,
    //format: on
  )

  val allSet: Set[Position] = all.toSet

  private val algebraicNotation2Position: Map[String, Position] = all.map(position => position.algebraicNotation -> position).toMap

  val byRank: Map[Int, List[Position]] = all.groupBy(_.rankInt)//.view.mapValues(_.sortBy(_.fileInt)).toMap

  val byFile: Map[Int, List[Position]] = all.groupBy(_.fileInt)//.view.mapValues(_.sortBy(_.rankInt)).toMap

  val byPositionTuple: Map[(Int, Int), Position] =
    all.map { case position @ Position(file, rank) =>
      (file, rank) -> position
    }.toMap

  def fromAlgebraicNotation(algebraicNotation: String): Either[Error, Position] =
    algebraicNotation2Position
      .get(algebraicNotation)
      .toRight(AlgebraicNotationParseError(algebraicNotation))

  def unapply(position: Position): Option[(Int, Int)] = Some(position.fileInt, position.rankInt)

  /*implicit def ordering(implicit tupleOrdering: Ordering[(Int, Int)]): Ordering[Position] =
    (x: Position, y: Position) => tupleOrdering.compare((-x.rankInt, x.fileInt), (-y.rankInt, y.fileInt))*/
}

case object A8 extends Position(0, 7)
case object B8 extends Position(1, 7)
case object C8 extends Position(2, 7)
case object D8 extends Position(3, 7)
case object E8 extends Position(4, 7)
case object F8 extends Position(5, 7)
case object G8 extends Position(6, 7)
case object H8 extends Position(7, 7)
case object A7 extends Position(0, 6)
case object B7 extends Position(1, 6)
case object C7 extends Position(2, 6)
case object D7 extends Position(3, 6)
case object E7 extends Position(4, 6)
case object F7 extends Position(5, 6)
case object G7 extends Position(6, 6)
case object H7 extends Position(7, 6)
case object A6 extends Position(0, 5)
case object B6 extends Position(1, 5)
case object C6 extends Position(2, 5)
case object D6 extends Position(3, 5)
case object E6 extends Position(4, 5)
case object F6 extends Position(5, 5)
case object G6 extends Position(6, 5)
case object H6 extends Position(7, 5)
case object A5 extends Position(0, 4)
case object B5 extends Position(1, 4)
case object C5 extends Position(2, 4)
case object D5 extends Position(3, 4)
case object E5 extends Position(4, 4)
case object F5 extends Position(5, 4)
case object G5 extends Position(6, 4)
case object H5 extends Position(7, 4)
case object A4 extends Position(0, 3)
case object B4 extends Position(1, 3)
case object C4 extends Position(2, 3)
case object D4 extends Position(3, 3)
case object E4 extends Position(4, 3)
case object F4 extends Position(5, 3)
case object G4 extends Position(6, 3)
case object H4 extends Position(7, 3)
case object A3 extends Position(0, 2)
case object B3 extends Position(1, 2)
case object C3 extends Position(2, 2)
case object D3 extends Position(3, 2)
case object E3 extends Position(4, 2)
case object F3 extends Position(5, 2)
case object G3 extends Position(6, 2)
case object H3 extends Position(7, 2)
case object A2 extends Position(0, 1)
case object B2 extends Position(1, 1)
case object C2 extends Position(2, 1)
case object D2 extends Position(3, 1)
case object E2 extends Position(4, 1)
case object F2 extends Position(5, 1)
case object G2 extends Position(6, 1)
case object H2 extends Position(7, 1)
case object A1 extends Position(0, 0)
case object B1 extends Position(1, 0)
case object C1 extends Position(2, 0)
case object D1 extends Position(3, 0)
case object E1 extends Position(4, 0)
case object F1 extends Position(5, 0)
case object G1 extends Position(6, 0)
case object H1 extends Position(7, 0)