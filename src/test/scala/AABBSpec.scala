import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import math.{AABB, Ray}
import org.specs2.{ScalaCheck, Specification}

class AABBSpec extends Specification with ScalaCheck {
  def is = s2"""
   An AABB should
      be intersected frontally $intersectFrontal
      be intersected frontally from Inside $intersectFrontalFromInside
      be intersected frontally from the side $intersectFrontalFromSide
      be missed frontally $missFrontal

  """

  val aabb = AABB(.5f, 2, .5f, 2, .5f, 2)

  def intersectFrontal = {
    val hit = aabb.intersect(Ray(VectorBreeze3.from(1, 1, 0), Z))
    (hit shouldNotEqual None) and
      (hit.get.normal shouldEqual VectorBreeze3.from(0, 0, -1f))
  }

  def intersectFrontalFromInside = {
    val hit = aabb.intersect(Ray(ONE, Z))
    (hit shouldNotEqual None) and
      (hit.get.normal shouldEqual VectorBreeze3.from(0, 0, -1f))
  }

  def intersectFrontalFromSide = {
    val pos = VectorBreeze3.from(-1, 2, 0)
    val ray = Ray(pos, normalized(VectorBreeze3.from(1, 1, .5f) - pos))
    val hit = aabb.intersect(ray)
    (hit shouldNotEqual None) and
      (hit.get.normal shouldEqual VectorBreeze3.from(0, 0, -1f))
  }

  def missFrontal = {
    val hit = aabb.intersect(Ray(origin = ZERO, direction = Z))
    hit should be equalTo None
  }

}
