import bounding.BVH
import math.{Ray, Shape, Sphere, Vector3}
import org.specs2.{ScalaCheck, Specification}
import renderer.Hit

class BVHSpec extends Specification with ScalaCheck {
  def is = s2"""
   An BVH should
      be constructed with correct height and size ${correctHeightAndSize}
      hit spheres correctly $intersectionSpheres

  """

  val spheres = for {
    x <- 1 to 3
    y <- 1 to 3
    z <- 1 to 3
  } yield Sphere(Vector3(x, y, z), radius = 0.2f)

  val sphereBVH = BVH(spheres, 2)

  def intersect(shapes: Seq[Shape], ray: Ray): Option[Hit] = {
    shapes.flatMap { s =>
      s intersect ray
    } match {
      case Nil => None
      case xs => Some(xs.minBy(_.distance))
    }
  }

  def correctHeightAndSize = {
    sphereBVH.size should be equalTo 9
    sphereBVH.depth should be equalTo 4
  }

  def intersectionSpheres = {
    val ray = Ray(origin = Vector3(2, 2, -10), direction = Vector3.Z)
    sphereBVH.intersect(ray) should be equalTo intersect(spheres, ray)
  }
}
