package chess.printers

import cats.effect.{IO, Resource}
import cats.syntax.parallel._
import chess.printers.ImageGamePrinter._
import chess._
import chess.printers.GamePrinter.PrinterFactory

import java.awt._
import java.awt.image.BufferedImage
import java.io.{File, InputStream}
import javax.imageio.ImageIO
import javax.swing._
import cats.effect.unsafe.implicits.global

private case class Images(
  whitePawn: BufferedImage,
  blackPawn: BufferedImage,
  whiteKnight: BufferedImage,
  blackKnight: BufferedImage,
  whiteBishop: BufferedImage,
  blackBishop: BufferedImage,
  whiteRook: BufferedImage,
  blackRook: BufferedImage,
  whiteQueen: BufferedImage,
  blackQueen: BufferedImage,
  whiteKing: BufferedImage,
  blackKing: BufferedImage,
  background: BufferedImage,
  highlight: BufferedImage,
)

case class ImageGamePrinter(
  game: Game,
  image: BufferedImage,
) extends GamePrinter[BufferedImage] {

  private def pieceImage(piece: Piece): BufferedImage = piece match {
    case Pawn(colour, _, _)   => colour.perColour(images.whitePawn)(images.blackPawn)
    case Knight(colour, _, _) => colour.perColour(images.whiteKnight)(images.blackKnight)
    case Bishop(colour, _, _) => colour.perColour(images.whiteBishop)(images.blackBishop)
    case Rook(colour, _, _)   => colour.perColour(images.whiteRook)(images.blackRook)
    case Queen(colour, _, _)  => colour.perColour(images.whiteQueen)(images.blackQueen)
    case King(colour, _, _)   => colour.perColour(images.whiteKing)(images.blackKing)
  }

  private def positionCoordinates(position: Position): (Int, Int) = {
    val x = position.fileInt * 150
    val y = 1050 - (position.rankInt * 150)
    (x, y)
  }

  private def drawImages(images: Set[(Position, BufferedImage)]): GamePrinter[BufferedImage] = {
    val graphics = image.createGraphics
    images.foreach { case (position, image) =>
      val (x, y) = positionCoordinates(position)
      graphics.drawImage(image, x, y, null)
    }
    graphics.dispose()
    this
  }

  override def withHighlights(highlightedPositions: Set[Position]): GamePrinter[BufferedImage] =
    drawImages(highlightedPositions.map(_ -> images.highlight))

  override def withIcons: GamePrinter[BufferedImage] =
    drawImages(game.board.pieces.view.mapValues(pieceImage).toSet)

  override def withHeader: GamePrinter[BufferedImage] = this

  override def withFooter(footer: String): GamePrinter[BufferedImage] = this

  override def withMoves: GamePrinter[BufferedImage] = this

  override def create: BufferedImage = image

  override def show: IO[Unit] = IO {
    val frame = new JFrame()
    frame.setLayout(new FlowLayout())
    val label = new JLabel(new ImageIcon(image))
    frame.add(label)
    frame.pack()
    frame.setVisible(true)
    frame.setResizable(false)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  }
}

object ImageGamePrinter {
  def apply(game: Game): ImageGamePrinter = new ImageGamePrinter(game, readImage("background.png").unsafeRunSync())
  implicit val factory: PrinterFactory[BufferedImage] = (game: Game) => ImageGamePrinter(game)

  private def resourceStream(name: String): InputStream =
    Thread.currentThread().getContextClassLoader.getResourceAsStream(name)

  private def readImage(name: String): IO[BufferedImage] = {
    val inputStream = resourceStream(name)
    IO(ImageIO.read(inputStream))
  }

  private def writeImage(image: BufferedImage, name: String): IO[Unit] =
    IO(ImageIO.write(image, "PNG", new File(name))).void

  private lazy val whitePawn: Resource[IO, BufferedImage] = Resource.make(readImage("wp.png"))(_ => IO.unit)
  private lazy val blackPawn: Resource[IO, BufferedImage] = Resource.make(readImage("bp.png"))(_ => IO.unit)
  private lazy val whiteKnight: Resource[IO, BufferedImage] = Resource.make(readImage("wn.png"))(_ => IO.unit)
  private lazy val blackKnight: Resource[IO, BufferedImage] = Resource.make(readImage("bn.png"))(_ => IO.unit)
  private lazy val whiteBishop: Resource[IO, BufferedImage] = Resource.make(readImage("wb.png"))(_ => IO.unit)
  private lazy val blackBishop: Resource[IO, BufferedImage] = Resource.make(readImage("bb.png"))(_ => IO.unit)
  private lazy val whiteRook: Resource[IO, BufferedImage] = Resource.make(readImage("wr.png"))(_ => IO.unit)
  private lazy val blackRook: Resource[IO, BufferedImage] = Resource.make(readImage("br.png"))(_ => IO.unit)
  private lazy val whiteQueen: Resource[IO, BufferedImage] = Resource.make(readImage("wq.png"))(_ => IO.unit)
  private lazy val blackQueen: Resource[IO, BufferedImage] = Resource.make(readImage("bq.png"))(_ => IO.unit)
  private lazy val whiteKing: Resource[IO, BufferedImage] = Resource.make(readImage("wk.png"))(_ => IO.unit)
  private lazy val blackKing: Resource[IO, BufferedImage] = Resource.make(readImage("bk.png"))(_ => IO.unit)
  private lazy val background: Resource[IO, BufferedImage] = Resource.make(readImage("background.png"))(_ => IO.unit)
  private lazy val highlight: Resource[IO, BufferedImage] = Resource.make(readImage("highlight.png"))(_ => IO.unit)

  lazy val resource: Resource[IO, Unit] =
    (
      whitePawn,
      blackPawn,
      whiteKnight,
      blackKnight,
      whiteBishop,
      blackBishop,
      whiteRook,
      blackRook,
      whiteQueen,
      blackQueen,
      whiteKing,
      blackKing,
      background,
      highlight,
    ).parMapN(Images.apply).map(images = _)

  private var images: Images = _
}
