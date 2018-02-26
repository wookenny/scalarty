package math

final case class Vector3(x: Double, y: Double, z: Double) {

  def /(s: Double): Vector3 = map(_/s)
  def *(s: Double): Vector3 = map(s*)

  def +(p: Vector3) = Vector3(x + p.x, y + p.y, z + p.z)
  def -(p: Vector3) = Vector3(x - p.x, y - p.y, z - p.z)
  def *(p: Vector3): Double = x * p.x + y * p.y + z * p.z
  def mult(p: Vector3): Vector3 = Vector3(x*p.y,y*p.y,z*p.z)

  def unary_-(): Vector3 = map(-1*)
  def unary_+(): Vector3 = this
  def ~=(p: Vector3, delta: Double = 0.001): Boolean = (this - p).length < delta

  lazy val length: Double = scala.math.sqrt(this * this)
  def dist(p: Vector3): Double = (this - p).length
  lazy val normalized: Vector3 = this / length
  def cross(p: Vector3) =
    Vector3(y * p.z - z * p.y, z * p.x - x * p.z, x * p.y - y * p.x)

  def pow(exp: Double) =
    Vector3(scala.math.pow(x, exp), scala.math.pow(y, exp), scala.math.pow(z, exp))
  def expf =
    Vector3(scala.math.exp(x), scala.math.exp(y), scala.math.exp(z))

  def map(f: Double => Double) = Vector3(f(x),f(y),f(z))

}

object Vector3 {
  val ZERO = Vector3(0, 0, 0)
  val ONE = Vector3(1, 1, 1)
  val X = Vector3(1, 0, 0)
  val Y = Vector3(0, 1, 0)
  val Z = Vector3(0, 0, 1)
}
