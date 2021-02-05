package chess

object Notation {

  val iconReplacements: Map[String, String] = Map(
    ((c: Colour) => King(c, A1)) -> "K",
    ((c: Colour) => Queen(c, A1)) -> "Q",
    ((c: Colour) => Rook(c, A1)) -> "R",
    ((c: Colour) => Bishop(c, A1)) -> "B",
    ((c: Colour) => Knight(c, A1)) -> "N",
  ).flatMap { case (builder, letter) =>
    List(White, Black).map(builder(_).icon -> letter)
  }

  private def convertIconsToString(string: String) =
    iconReplacements.foldLeft(string) { case (string, replacement) =>
      (string.replaceAll _).tupled(replacement)
    }

  def convertFromPGN(moves: String): List[String] =
    moves
      .replaceAll("\\d+\\. ", "") //removeTurnNumbers
      .replaceAll("\\$12", "")    //Encoded =
      .replaceAll("=", "")        //Non Encoded =
      .split(" ")                 // split into single moves
      .map(convertIconsToString)
      .toList

}
