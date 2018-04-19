package color

import generators.Generators._
import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.Prop.forAll
import org.specs2.mock.Mockito

class RGBSpec extends Specification with ScalaCheck with Mockito{
  def is =
    s2"""
   An RGB should
      be added correctly to another RGB $addCorrectly
      be subtracted correctly from another RGB $subtractCorrectly
      be multiplied correctly with another RGB $multiplyCorrectly
      be multiplied correctly with a scalar $multiplyScalarCorrectly
      be divided correctly with a scalar $divideCorrectly

      be negated correctly $negateCorrectly
      not change with unary plus $positiveCorrectly

      calculate correct awt colors in the correct range $awtColor
      expose any value between 0 and 1 $testExposure
      calculate the power correctly $testPower
      gamma correct its values accordingly $testGammaCorrection
      map its values according a given function correctly $testMapping
     """

  private def toSeq(c: RGB): Seq[Double] = Seq(c.red, c.green, c.blue)


  def addCorrectly = forAll(rgb, rgb){ (a: RGB, b: RGB) =>
    a + b should be equalTo RGB(a.red + b.red, a.green + b.green, a.blue + b.blue)
  }

  def subtractCorrectly = forAll(rgb, rgb){ (a: RGB, b: RGB) =>
    a - b should be equalTo RGB(a.red - b.red, a.green - b.green, a.blue - b.blue)
  }

  def multiplyCorrectly = forAll(rgb, rgb) { (a: RGB, b: RGB) =>
    a mult b should be equalTo RGB(a.red * b.red, a.green * b.green, a.blue * b.blue)
  }

  def multiplyScalarCorrectly = forAll(rgb, positiveDouble) { (a: RGB, s: Double) =>
    a * s should be equalTo RGB(a.red * s, a.green * s, a.blue * s)
  }

  def divideCorrectly = forAll(rgb, positiveDouble) { (a: RGB, s: Double) =>
    a / s should be equalTo RGB(a.red / s, a.green / s, a.blue / s)
  }

  def negateCorrectly = forAll(rgb) { (a: RGB) =>
    -a should be equalTo RGB(-a.red, -a.green, -a.blue)
  }
  def positiveCorrectly = forAll(rgb) { (a: RGB) =>
    +a should be equalTo a
  }

  def awtColor = forAll(rgb) { (a: RGB) =>
    val awtColor = a.awtColor
    val expectedColors = toSeq(a)
      .map { c =>
        Math.max(Math.min(c, 1), 0)
      }
      .map { c =>
        255 * c
      }
    val awtSeq =
      Seq(awtColor.getRed.toDouble, awtColor.getGreen.toDouble, awtColor.getBlue.toDouble)
    val maxDifference =
      awtSeq.zip(expectedColors).map { case (x, y) => x - y }.max
    maxDifference should be lessThanOrEqualTo 1
  }

  def testExposure = forAll(rgb) { (a: RGB) =>
    val exposure = a.exposureCorrected
    val values = toSeq(exposure)
    (values.max should be lessThanOrEqualTo 1) and
      (values.min should be greaterThanOrEqualTo 0)
  }

  def testPower  = forAll(rgb) { (a: RGB) =>
    (a ^ 0 should be equalTo RGB(1d, 1d, 1d)) and
      (a ^ 1 should be equalTo a) and
      (a ^ 2 should be equalTo RGB(a.red * a.red, a.green * a.green, a.blue * a.blue))
  }

  def testGammaCorrection = forAll(rgb) { (a: RGB) =>
    val expectedColor = RGB(Math.pow(a.red, 1 / RGB.GAMMA),
                            Math.pow(a.green, 1 / RGB.GAMMA),
                            Math.pow(a.blue, 1 / RGB.GAMMA))
    a.gammaCorrected should be equalTo expectedColor
  }

  def testMapping = forAll(rgb,threeDimensionalPoint) { (a: RGB, point: (Double,Double,Double)) =>

    val (r,g,b) = point
    val f = mock[(Double => Double)]
    f(a.red) returns r
    f(a.green) returns g
    f(a.blue) returns b

    a.map(f) should be equalTo RGB(r,g,b)
  }

}
