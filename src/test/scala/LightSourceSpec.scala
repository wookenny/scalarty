import color.RGB
import lightning.{LightSource, PlaneLight, PointLight}
import math.Vector3
import org.specs2.matcher.MatchResult
import org.specs2.{ScalaCheck, Specification}
import io.circe.{Decoder, Encoder, Error, Json}
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._

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
    PlaneLight(position = Vector3.ZERO, width = 0.2, length = 0.4, RGB.BLUE, power = 124)

  val pointLight = PointLight(position = Vector3.ZERO, RGB.RED, power = 12)

  val intensityTestPointlight = {
    val power1: Double = planeLight.intensity(Vector3.Z * 2)
    val power2: Double = planeLight.intensity(Vector3.X, Some(Vector3.X * 2))

    (power1 should be equalTo 124 / 4) and
      (power2 should be equalTo 124)

  }

  val intensityTestPlanelight = {
    val power: Double = pointLight.intensity(Vector3.Z * 2)
    val powerUnitDistance: Double = pointLight.intensity(Vector3.X)

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
          p.x >= planeLight.position.x - planeLight.width
            && p.x <= planeLight.position.x + planeLight.width
            && p.z >= planeLight.position.z - planeLight.length
            && p.z <= planeLight.position.z + planeLight.length
            && Math.abs(p.y - planeLight.position.y) < 0.001)
      .distinct
    (correctSizes should be equalTo true) and
      (positionOnLight should contain(exactly(true)))
  }

  val sampleTestPlanelight = {
    pointLight.sample(n = 4) should contain(exactly(Vector3.ZERO))
  }

  val parsePointlight = parseLight(pointLight)

  val parsePlanelight = parseLight(planeLight)

  val parsingUnkownLight = {
    val light = TestLight()
    parseLight(light) must throwAn[MatchError]
  }

  case class TestLight(color: RGB = RGB.CYAN, position: Vector3 = Vector3.ZERO)
      extends LightSource {
    override def intensity(p: Vector3, positionOnLight: Option[Vector3]): Double = 4
    override def sample(n: Int): Seq[Vector3] = Seq.empty
  }

  private def parseLight(in: LightSource) = {
    val js = in.asJson
    val out = decode[LightSource](js.toString)
    out should be equalTo Right(in)
  }
}
