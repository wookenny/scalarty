package generators

import color.RGB
import material.node.{AddValues, MixValues, _}
import math.Vector3
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object Generators {

  val smallPositiveDouble = Gen.choose(0.1d, 200d)
  val positiveDouble = Gen.choose(0f, Double.MaxValue)
  val generalDouble = Gen.choose(Double.MinValue, Double.MaxValue)
  val smallMultipleOfaQuarter =  Gen.choose(1, 20).flatMap(_ * 0.25)


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
    x <- Gen.choose(-10^6, 10^6)
    y <- Gen.choose(-10^6, 10^6)
    z <- Gen.choose(-10^6, 10^6)
  } yield Vector3(x, y, z)

  val smallVector3 = for {
    x <- Gen.choose(0.1, 10d)
    y <- Gen.choose(0.1, 10d)
    z <- Gen.choose(0.1, 10d)
  } yield Vector3(x, y, z)

  val smallPositiveIntegerVector3 = for {
    x <- Gen.choose(1, 10)
    y <- Gen.choose(1, 10)
    z <- Gen.choose(1, 10)
  } yield Vector3(x, y, z)



  val rgb = for {
    r: Double <- positiveDouble
    g: Double <- positiveDouble
    b: Double <- positiveDouble
  } yield RGB(r, g, b)


}
