import math._
import org.specs2.Specification
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax._

class ShapeSpec extends Specification {
  def is =
    s2"""
       A Shape should
          be  capable of parsing a Sphere as a Json $parseSphere
    """

  val parseSphere = parseShape(Sphere(Vector3(1, 2, 3.14159), 2.3f))
  val parseTriangle = parseShape(Triangle(Vector3(1, 2, 0), Vector3(1, 2, 3), Vector3(2, 1, 4.2)))
  val parseAABB = parseShape(NonEmptyAABB(-1.1f, 1, -0.2f, -0.01f, 10, 12.3f))

  private def parseShape(in: Shape) = {
    val js: Json = in.asJson
    val out = decode[Shape](js.toString)
    out should be equalTo Right(in)
  }

}

//see: http://stackoverflow.com/questions/17021847/noise-free-json-format-for-sealed-traits-with-play-2-2-library
