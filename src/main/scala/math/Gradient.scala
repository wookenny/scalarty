package math

import color.RGB

import scala.language.implicitConversions

object Gradient {

  trait Gradientable[T] {
    def -(other: T): T
    def /(scalar: Double): T
  }

  private val EPS = 0.01

  def gradient[T](
      x: Double
  )(implicit function: OneDimensionalFunction[T], op: T => Gradientable[T]): T = {
    (function.eval(x + EPS) - function.eval(x - EPS)) / (2 * EPS)
  }

  def gradient[T](
      x: Double,
      y: Double
  )(implicit function: TwoDimensionalFunction[T], op: T => Gradientable[T]): (T, T) = {
    (
      (function.eval(x + EPS, y) - function.eval(x - EPS, y)) / (2 * EPS),
      (function.eval(x, y + EPS) - function.eval(x, y - EPS)) / (2 * EPS)
    )
  }

  def gradient[T](x: Double, y: Double, z: Double)(
      implicit function: ThreeDimensionalFunction[T],
      op: T => Gradientable[T]
  ): (T, T, T) = {
    (
      (function.eval(x + EPS, y, z) - function.eval(x - EPS, y, z)) / (2 * EPS),
      (function.eval(x, y + EPS, z) - function.eval(x, y - EPS, z)) / (2 * EPS),
      (function.eval(x, y, z + EPS) - function.eval(x, y, z - EPS)) / (2 * EPS)
    )
  }
  def gradient[T](x: Double, y: Double, z: Double, w: Double)(
      implicit function: FourDimensionalFunction[T],
      op: T => Gradientable[T]
  ): (T, T, T, T) = {

    (
      (function.eval(x + EPS, y, z, w) - function.eval(x - EPS, y, z, w)) / (2 * EPS),
      (function.eval(x, y + EPS, z, w) - function.eval(x, y - EPS, z, w)) / (2 * EPS),
      (function.eval(x, y, z + EPS, w) - function.eval(x, y, z - EPS, w)) / (2 * EPS),
      (function.eval(x, y, z, w + EPS) - function.eval(x, y, z, w - EPS)) / (2 * EPS)
    )
  }

  object implicits {
    implicit val gradientableDouble: Double => Gradientable[Double] = value =>
      new Gradientable[Double] {
        override def -(other: Double): Double = value - other

        override def /(scalar: Double): Double = value / scalar
      }

    implicit val gradientableRGB: RGB => Gradientable[RGB] = value =>
      new Gradientable[RGB] {
        override def -(other: RGB): RGB = value - other

        override def /(scalar: Double): RGB = value / scalar
      }

    implicit val gradientableVector3: Vector3 => Gradientable[Vector3] = value =>
      new Gradientable[Vector3] {
        override def -(other: Vector3): Vector3 = value - other

        override def /(scalar: Double): Vector3 = value / scalar
      }
  }

}
