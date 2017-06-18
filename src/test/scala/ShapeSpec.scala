import math._
import org.specs2.{ScalaCheck, Specification}
import play.api.libs.json.Json

class ShapeSpec extends Specification {
  def is =
    s2"""
       A Shape should
          be  capable of parsing a Sphere as a Json $parseSphere
    """

  val parseSphere = parseShape(Sphere(Vector3(1, 2, 3.14159), 2.3f))
  val parseTriangle = parseShape(Triangle(Vector3(1, 2, 0), Vector3(1, 2, 3), Vector3(2, 1, 4.2)))
  val parseAABB = parseShape(AABB(-1.1f, 1, -0.2f, -0.01f, 10, 12.3f))

  //TODO make an own matcher with type T out of it
  private def parseShape(in: Shape) = {
    val js = Json.toJson(in)
    val out = Json.fromJson[Shape](js).get
    out should be equalTo in
  }

}

//see: http://stackoverflow.com/questions/17021847/noise-free-json-format-for-sealed-traits-with-play-2-2-library
