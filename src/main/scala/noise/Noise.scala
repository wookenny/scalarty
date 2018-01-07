package noise

trait Noise {
  val seed: Long

  def value(x: Double, y: Double): Double
  def value(x: Double, y: Double, z: Double): Double
  def value(x: Double, y: Double, z: Double, w: Double): Double
}
