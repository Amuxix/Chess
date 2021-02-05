package chess

trait Attacker {
  def attacker: Colour
  lazy val defender: Colour = attacker.other
}
