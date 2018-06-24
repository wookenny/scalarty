import math.{AABB, Cuboid, Ray, Vector3}
import org.specs2.{ScalaCheck, Specification}

class CuboidSpec extends Specification with ScalaCheck {
  def is = s2"""
   A Cuboid should
      be intersected frontally $intersectFrontal
      be intersected frontally from Inside $intersectFrontalFromInside
      be intersected frontally from the side $intersectFrontalFromSide
      be missed frontally $missFrontal

  """

  val cuboid =
    Cuboid(center = Vector3.ZERO, sideLengths = Vector3(1, 1.5, 4), rotation = Vector3(0, 90, 0))

  private def intersectFrontal = {
    val hit = cuboid.intersect(Ray(Vector3(0.5, 0.5, -1f), Vector3.Z))
    (hit shouldNotEqual None) and
      (hit.get.normal ~= Vector3(0, 0, -1f)) and
      (hit.get.position ~= Vector3(0.5, 0.5, -0.5))
  }

  private def intersectFrontalFromInside = {
    val hit = cuboid.intersect(Ray(Vector3.ZERO, Vector3.Z))
    (hit shouldNotEqual None) and
      (hit.get.normal ~= Vector3(0, 0, -1f))
  }

  private def intersectFrontalFromSide = {
    val pos = Vector3(0,0,-6)
    val ray = Ray(pos, Vector3.Z)
    val hit = cuboid.intersect(ray)
    (hit shouldNotEqual None) and
      (hit.get.normal ~= Vector3(0, 0, -1f))
  }

  private def missFrontal = {
    val hit = cuboid.intersect(Ray(origin = Vector3(5,5,-2), direction = Vector3.Z))
    hit should be equalTo None
  }

}
