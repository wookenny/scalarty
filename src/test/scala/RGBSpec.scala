import color.RGB
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, Specification}


class RGBSpec  extends Specification with ScalaCheck {
  def is =
    s2"""
   An RGB should
      be added correctly to another RGB $addCorrectly
      be subtracted correctlyfrom another RGB $subtractCorrectly
      be multiplied correctly with a scalar $multiplyCorrectly
      be divided correctly with a scalar $divideCorrectly

      be negated correctly $negateCorrectly
      not change with unary plus $positiveCorrectly

      calculate correct awt colors in the correct range $awtColor
      expose any value between 0 and 1 $testExposure
      calculate the power correctly $testPower
      gamma correct its values accordingly $testGammaCorrection
     """

  def toSeq(c: RGB): Seq[Double] = Seq(c.red, c.green, c.blue)

  implicit lazy val RGBGenerator: Arbitrary[RGB] =
    Arbitrary {
      for {r: Double <- DoubleGenerator.arbitrary
           g: Double <- DoubleGenerator.arbitrary
           b: Double <- DoubleGenerator.arbitrary
      } yield RGB(r, g, b)
    }

  implicit lazy val DoubleGenerator: Arbitrary[Double] =
    Arbitrary {
      Gen.choose(0f, Double.MaxValue)
    }

  val addCorrectly: Prop = forAll { (a: RGB, b: RGB) => a + b should be equalTo RGB(a.red + b.red, a.green + b.green, a.blue + b.blue) }
  val subtractCorrectly: Prop = forAll { (a: RGB, b: RGB) => a - b should be equalTo RGB(a.red - b.red, a.green - b.green, a.blue - b.blue) }
  val multiplyCorrectly: Prop = forAll { (a: RGB, s: Double) => a * s should be equalTo RGB(a.red * s, a.green * s, a.blue * s) }
  val divideCorrectly: Prop = forAll { (a: RGB, s: Double) => a / s should be equalTo RGB(a.red / s, a.green / s, a.blue / s) }

  val negateCorrectly: Prop = forAll { (a: RGB) => -a should be equalTo RGB(-a.red, -a.green, -a.blue) }
  val positiveCorrectly: Prop = forAll { (a: RGB) => +a should be equalTo a }

  val awtColor: Prop = forAll { (a: RGB) => val awtColor = a.awtColor
    val expectedColors = toSeq(a).map { c => Math.max(Math.min(c, 1), 0) }.map { c => 255 * c }
    val awtSeq = Seq(awtColor.getRed.toDouble, awtColor.getGreen.toDouble, awtColor.getBlue.toDouble)
    val maxDifference = awtSeq.zip(expectedColors).map { case (x, y) => x - y }.max
    maxDifference should be lessThanOrEqualTo (1)
  }


  val testExposure: Prop = forAll { (a: RGB) => val exposure = a.exposureCorrected
    val values = toSeq(exposure)
    (values.max should be lessThanOrEqualTo (1)) and
      (values.min should be greaterThanOrEqualTo (0))
  }

  val testPower: Prop = forAll { (a: RGB) => (a ^ 0 should be equalTo RGB(1f, 1f, 1f)) and
    (a ^ 1 should be equalTo a) and
    (a ^ 2 should be equalTo RGB(a.red * a.red, a.green * a.green, a.blue * a.blue))
  }

  val testGammaCorrection: Prop = forAll { (a: RGB) => val expectedColor =  RGB(Math.pow(a.red,1/RGB.GAMMA).toDouble,
                                                                                Math.pow(a.green,1/RGB.GAMMA).toDouble,
                                                                                Math.pow(a.blue,1/RGB.GAMMA).toDouble)
                                            a.gammaCorrected should be equalTo expectedColor
  }

  }