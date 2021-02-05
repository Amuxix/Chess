package chess

trait Testt {

  def a: Int
  implicit val b: Int = a
}

class A(val a: Int) extends Testt {
  def summoned: Int = A.summon
}

object A {
  def summon(implicit b: Int): Int = b
  val a = new A(1)
}
