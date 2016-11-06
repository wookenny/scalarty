import math.{AABB, Ray, Vector3}
import org.specs2.{ScalaCheck, Specification}


class AABBSpec  extends Specification with ScalaCheck { def is = s2"""
   An math.AABB should
      be intersected frontally ${intersectFrontal}
      be intersected frontally from Inside ${intersectFrontalFromInside}
      be intersected frontally from the side $intersectFrontalFromSide
      be missed frontally ${missFrontal}

  """

   val aabb = AABB(.5f,2, .5f,2, .5f,2)

  def intersectFrontal = {
    val hit = aabb.intersect(Ray(Vector3(1,1,0), Vector3.Z))
    (hit shouldNotEqual None) and
      (hit.get.normal shouldEqual(Vector3(0,0,-1f)))
  }

  def intersectFrontalFromInside = {
    val hit = aabb.intersect(Ray(Vector3.ONE, Vector3.Z))
    (hit shouldNotEqual None) and
      (hit.get.normal shouldEqual(Vector3(0,0,-1f)))
  }

  def intersectFrontalFromSide = {
    val pos = Vector3(-1,2,0)
    val ray = Ray(pos, (Vector3(1,1,.5f) - pos).normalized)
    val hit = aabb.intersect(ray)
    (hit shouldNotEqual None) and
      (hit.get.normal shouldEqual(Vector3(0,0,-1f)))
  }

  def missFrontal = {
    val hit = aabb.intersect(Ray(origin = Vector3.ZERO, direction =  Vector3.Z))
    hit should be equalTo None
  }

}
