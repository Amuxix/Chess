package chess

sealed trait Colour {
  def other: Colour

  def perColour[A](white: => A)(black: => A): A =
    this match {
      case White => white
      case Black => black
    }
}

case object White extends Colour {
  override lazy val other: Colour = Black
}

case object Black extends Colour {
  override lazy val other: Colour = White
}
