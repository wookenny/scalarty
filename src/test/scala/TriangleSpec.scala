import math.{AABB, Ray, Triangle, Vector3}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}


class TriangleSpec extends Specification with ScalaCheck {
  def is = s2"""
   A triangle should
      be intersected from both sides $intersectFrontal
      be intersected accordingly to distance $intersectAccordingToDistance
      be intersected frontally correctly ${hittingTest.verbose}
      be intersected frontally with large enough distance ${hittingTestDist.verbose}
      be missed frontally due to short distance ${hittingTestDist.verbose}
      be missed $missingTest
      calculate interopoalted Normals $todo


      return the minimal axis parallel bounding box $boundingBoxTest
      return its midpoint $midpointTest
  """

  def a = Vector3(-1, -1, 1)
  def b = Vector3(1, -1, 1)
  def c = Vector3(1, 1, 1)

  def triangle = Triangle(a, b, c)

  def intersectFrontal = {

    val hitFront = triangle.intersect(Ray(Vector3(-0.2, -0.2, 0), Vector3.Z))
    val hitBack = triangle.intersect(Ray(Vector3(-0.2, -0.2, 2), -Vector3.Z))

    (hitFront shouldNotEqual None) and (hitBack shouldNotEqual None)
  }

  def intersectAccordingToDistance = {
    val hitFront =
      triangle.intersect(Ray(Vector3(-0.2, -0.2, 0), Vector3.Z), 1 + .1)
    val missFront =
      triangle.intersect(Ray(Vector3(-0.2, -0.2, 0), -Vector3.Z), 1 - .1)

    val hitBack =
      triangle.intersect(Ray(Vector3(-0.2, -0.2, 2), -Vector3.Z), 1 + .1)
    val missBack =
      triangle.intersect(Ray(Vector3(-0.2, -0.2, 2), -Vector3.Z), 1 - .1)

    (hitFront, hitBack, missFront, missBack) should beEqualTo((true, true, false, false))
  }

  val hittingTest = forAll { (r: Ray) =>
    triangle.intersect(r).isDefined
  }

  val hittingTestDist = forAll { (r: Ray) =>
    triangle.intersect(r, 1.1)
  }

  val missingTestDist = forAll { (r: Ray) =>
    !triangle.intersect(r, .9)
  }

  val missingTest = {
    val miss1 = triangle.intersect(Ray(Vector3(-2, -2, 0), Vector3.Z), 2)
    val miss2 = triangle.intersect(Ray(Vector3( 2,  2, 0), Vector3.Z), 2)
    val miss4 = triangle.intersect(Ray(Vector3(-2, -2, 0), Vector3.Z)).isDefined
    val miss5 = triangle.intersect(Ray(Vector3( 2,  2, 0), Vector3.Z)).isDefined

    (miss1, miss2) should beEqualTo (false,false)
  }

  def boundingBoxTest = forAll { (a: Vector3, b: Vector3, c: Vector3) =>
    val aabb = Triangle(a, b, c).boundingBox
    val xs = Seq(a,b,c).map(_.x)
    val ys = Seq(a,b,c).map(_.y)
    val zs = Seq(a,b,c).map(_.z)

    aabb == AABB(xs.min, xs.max, ys.min, ys.max, zs.min, zs.max )
  }

  def midpointTest =  forAll { (a: Vector3, b: Vector3, c: Vector3) =>
    Triangle(a,b,c).midpoint ~= a/3+b/3+c/3
  }


  implicit lazy val Vector3Gen: Arbitrary[Vector3] =
    Arbitrary {
      for {
        x: Double <- Gen.choose(-1000d, 1000d)
        y: Double <- Gen.choose(-1000d, 1000d)
        z: Double <- Gen.choose(-1000d, 1000d)
      } yield Vector3(x, y, z)
    }

  implicit lazy val HittingRayGen: Arbitrary[Ray] =
    Arbitrary {
      for {
        x: Double <- Gen.choose(-.9, .9)
        y: Double <- Gen.choose(-.9, x)
      } yield Ray(Vector3(x, y, 0), Vector3.Z)
    }

}
