package generators

import color.RGB
import org.scalacheck.Gen


object Generators {

  val positiveDouble = Gen.choose(0f, Double.MaxValue)
  val generalDouble  = Gen.choose(Double.MinValue, Double.MaxValue)


  val regularRgb = for {
    x <- Gen.choose(0d, 1d)
    y <- Gen.choose(0d, 1d)
    z <- Gen.choose(0d, 1d)
  } yield RGB(x, y, z)

  val threeDimensionalPoint = for {
    x <- Gen.choose(Double.MinValue, Double.MaxValue)
    y <- Gen.choose(Double.MinValue, Double.MaxValue)
    z <- Gen.choose(Double.MinValue, Double.MaxValue)
  } yield (x, y, z)

  val rgb = for {
    r: Double <- positiveDouble
    g: Double <- positiveDouble
    b: Double <- positiveDouble
  } yield RGB(r, g, b)


}
