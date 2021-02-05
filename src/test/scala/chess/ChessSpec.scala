package chess

import org.scalatest.{EitherValues => ScalaTestEitherValues}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class ChessSpec extends AnyWordSpec with Matchers with ScalaTestEitherValues with EitherValues
