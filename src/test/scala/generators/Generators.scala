package generators

import color.RGB
import material.node.{AddValues, MixValues, _}
import math.Vector3
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object Generators {

  val positiveDouble = Gen.choose(0f, Double.MaxValue)
  val generalDouble = Gen.choose(Double.MinValue, Double.MaxValue)

  val regularRgb = for {
    x <- Gen.choose(0d, 1d)
    y <- Gen.choose(0d, 1d)
    z <- Gen.choose(0d, 1d)
  } yield RGB(x, y, z)

  val threeDimensionalPoint = for {
    x <- arbitrary[Double]
    y <- arbitrary[Double]
    z <- arbitrary[Double]
  } yield (x, y, z)

  val vector3 = for {
    x <- arbitrary[Double]
    y <- arbitrary[Double]
    z <- arbitrary[Double]
  } yield Vector3(x, y, z)

  val rgb = for {
    r: Double <- positiveDouble
    g: Double <- positiveDouble
    b: Double <- positiveDouble
  } yield RGB(r, g, b)

}
