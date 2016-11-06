package math

object Math {
  def sqrt(f: Float) = scala.math.sqrt(f).toFloat
  def min(a: Float, b: Float) = scala.math.min(a, b)
  def max(a: Float, b: Float) = scala.math.max(a, b)

  val EPS: Float = 0.0001.toFloat
}
