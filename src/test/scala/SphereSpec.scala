import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import math.{Ray, Sphere}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, Specification}

class SphereSpec extends Specification with ScalaCheck {
  def is = s2"""
   A sphere shoul
     be intersected frontally §intersectFrontal
     be missed if sphere is behind the ray $missBackwards
     be intersected from the inside $insersectFromInside
     be intersected from the inside with given distance $insersectFromInsideWithCorrectDist
     not be intersected from the inside due to too short distance $missedFromInsideDueToDist
  """

  def sphere1 = Sphere(center = ZERO, radius = 4)

  val insersectFromInside: Prop = forAll { (r: Ray) =>
    sphere1.intersect(r) isDefined
  }
  val insersectFromInsideWithCorrectDist = forAll { (r: Ray) =>
    sphere1.intersect(r, 4.1)
  }
  val missedFromInsideDueToDist: Prop = forAll { (r: Ray) =>
    !sphere1.intersect(r, 3.9)
  }

  val intersectFrontal = forAll { (r: Ray) =>
    Sphere(center = r.march(9), .3).intersect(r) isDefined
  }
  val missBackwards = forAll { (r: Ray) =>
    Sphere(center = r.march(-9), .3).intersect(r) isEmpty
  }

  //TODO:  maybe just a vector3 generator!!
  implicit lazy val HittingRayGen: Arbitrary[Ray] =
    Arbitrary {
      for {
        x: Double <- Gen.choose(-.9, .9)
        y: Double <- Gen.choose(-.9, .9)
        z: Double <- Gen.choose(-.9, .9)
      } yield Ray(ZERO, normalized(VectorBreeze3.from(x, y, z)))
    }

}
