import color.RGB
import lightning.{LightSource, PlaneLight, PointLight}
import math.Vector3
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

  val planeLight = PlaneLight(position = Vector3.ZERO,
                              width = 0.2,
                              length= 0.4,
                              RGB.BLUE,
                              power = 0.123)

  val pointLight = PointLight(position = Vector3.ZERO,
                              RGB.RED,
                              power = 12)

  val intensityTestPointlight = {
    todo
  }

  val intensityTestPlanelight = {
    todo
  }

  val sampleTestPointlight = {
    todo
  }

  val sampleTestPlanelight = {
    todo
  }

  val parsePointlight = parseLight(pointLight)

  val parsePlanelight = parseLight(planeLight)

  val parsingUnkownLight = {
    val light = TestLight()
    parseLight(light) must throwAn[MatchError]
  }


  case class TestLight(color :RGB = RGB.CYAN, position : Vector3 = Vector3.ZERO) extends LightSource{
    override def intensity(p: Vector3, positionOnLight: Option[Vector3]): Double = 4
    override def sample(n: Int): Seq[Vector3] = Seq.empty
  }

  private def parseLight(in: LightSource) = {
    val js = Json.toJson(in)
    val out = Json.fromJson[LightSource](js).get
    out should be equalTo in
  }
}
