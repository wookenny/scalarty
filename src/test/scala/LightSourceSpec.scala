import color.RGB
import lightning.{LightSource, PlaneLight, PointLight}
import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import org.specs2.{ScalaCheck, Specification}
import play.api.libs.json.Json

class LightSourceSpec extends Specification with ScalaCheck {

  def is = s2"""
    A Pointlight should
      calculate intensity according to distance $intensityTestPlanelight
      sample light source $sampleTestPointlight
      be parsed correctly $parsePointlight

    A Planelight should
      calculate intensity according to distance $intensityTestPlanelight
      sample light source $sampleTestPlanelight
      be parsed correctly $parsePlanelight

    A Lightsorce should
      produce an error when aprsing an unknown type $parsingUnkownLight
    """

  val planeLight =
    PlaneLight(position = ZERO, width = 0.2, length = 0.4, RGB.BLUE, power = 124)

  val pointLight = PointLight(position = ZERO, RGB.RED, power = 12)

  val intensityTestPointlight = {
    val power1: Double = planeLight.intensity(Z * 2d)
    val power2: Double = planeLight.intensity(X, Some(X * 2d))

    (power1 should be equalTo 124 / 4) and
      (power2 should be equalTo 124)

  }

  val intensityTestPlanelight = {
    val power: Double = pointLight.intensity(Z * 2d)
    val powerUnitDistance: Double = pointLight.intensity(X)

    (power should be equalTo 3) and
      (powerUnitDistance should be equalTo 12)
  }

  val sampleTestPointlight = {
    val sample1 = planeLight.sample(n = 1).distinct
    val sample2 = planeLight.sample(n = 2).distinct
    val sample3 = planeLight.sample(n = 3).distinct

    val correctSizes = sample3.size == 9 && sample2.size == 4 && sample1.size == 1
    val positionOnLight = (sample1 ++ sample2 ++ sample3)
      .map(
        p =>
          p(0)>= planeLight.position(0) - planeLight.width
            && p(0) <= planeLight.position(0) + planeLight.width
            && p(2) >= planeLight.position(2) - planeLight.length
            && p(2) <= planeLight.position(2) + planeLight.length
            && Math.abs(p(1) - planeLight.position(1)) < 0.001)
      .distinct
    (correctSizes should be equalTo true) and
      (positionOnLight should contain(exactly(true)))
  }

  val sampleTestPlanelight = {
    pointLight.sample(n = 4) should contain(exactly(ZERO))
  }

  val parsePointlight = parseLight(pointLight)

  val parsePlanelight = parseLight(planeLight)

  val parsingUnkownLight = {
    val light = TestLight()
    parseLight(light) must throwAn[MatchError]
  }

  case class TestLight(color: RGB = RGB.CYAN, position: VectorBreeze3 = ZERO)
      extends LightSource {
    override def intensity(p: VectorBreeze3, positionOnLight: Option[VectorBreeze3]): Double = 4d
    override def sample(n: Int): Seq[VectorBreeze3] = Seq.empty
  }

  private def parseLight(in: LightSource) = {
    val js = Json.toJson(in)
    val out = Json.fromJson[LightSource](js).get
    out should be equalTo in
  }
}
