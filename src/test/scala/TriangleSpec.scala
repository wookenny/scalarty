import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import math.{AABB, Ray, Triangle}
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
      calculate interopoalted Normals §todo


      return the minimal axis parallel bounding box $boundingBoxTest
      return its midpoint $midpointTest
  """

  val a = VectorBreeze3.from(-1, -1, 1)
  val b = VectorBreeze3.from(1, -1, 1)
  val c = VectorBreeze3.from(1, 1, 1)

  val triangle = Triangle(a, b, c)

  implicit lazy val Vector3Gen: Arbitrary[VectorBreeze3] = Arbitrary {
    for {
      x: Double <- Gen.choose(-1000d, 1000d)
      y: Double <- Gen.choose(-1000d, 1000d)
      z: Double <- Gen.choose(-1000d, 1000d)
    } yield VectorBreeze3.from(x, y, z)
  }


  implicit lazy val HittingRayGen: Arbitrary[Ray] = Arbitrary {
    for {
      x: Double <- Gen.choose(-.9, .9)
      y: Double <- Gen.choose(-.9, x)
    } yield Ray(VectorBreeze3.from(x, y, 0), Z)
  }

  def intersectFrontal = {

    val hitFront = triangle.intersect(Ray(VectorBreeze3.from(-0.2, -0.2, 0), Z))
    val hitBack = triangle.intersect(Ray(VectorBreeze3.from(-0.2, -0.2, 2), -Z))

    (hitFront shouldNotEqual None) and (hitBack shouldNotEqual None)
  }

  def intersectAccordingToDistance = {
    val hitFront =
      triangle.intersect(Ray(VectorBreeze3.from(-0.2, -0.2, 0), Z), 1 + .1)
    val missFront =
      triangle.intersect(Ray(VectorBreeze3.from(-0.2, -0.2, 0), -Z), 1 - .1)

    val hitBack =
      triangle.intersect(Ray(VectorBreeze3.from(-0.2, -0.2, 2), -Z), 1 + .1)
    val missBack =
      triangle.intersect(Ray(VectorBreeze3.from(-0.2, -0.2, 2), -Z), 1 - .1)

    (hitFront, hitBack, missFront, missBack) should beEqualTo((true, true, false, false))
  }

  def hittingTest = forAll { (r: Ray) =>
    println(s"ray is $r")
    triangle.intersect(r).isDefined
  }

  def hittingTestDist = forAll { (r: Ray) =>
    triangle.intersect(r, 1.1)
  }

  def missingTestDist = forAll { (r: Ray) =>
    !triangle.intersect(r, .9)
  }

  def missingTest = {
    lazy val miss1 = triangle.intersect(Ray(VectorBreeze3.from(-2, -2, 0), Z), 2)
    lazy val miss2 = triangle.intersect(Ray(VectorBreeze3.from(2, 2, 0), Z), 2)
    (miss1, miss2) should beEqualTo(false, false)
  }

  def boundingBoxTest = forAll { (a: VectorBreeze3, b: VectorBreeze3, c: VectorBreeze3) =>
    val aabb = Triangle(a, b, c).boundingBox
    val xs = Seq(a, b, c).map(_(0))
    val ys = Seq(a, b, c).map(_(1))
    val zs = Seq(a, b, c).map(_(2))

    aabb == AABB(xs.min, xs.max, ys.min, ys.max, zs.min, zs.max)
  }

  def midpointTest = forAll { (a: VectorBreeze3, b: VectorBreeze3, c: VectorBreeze3) =>
    ~=( Triangle(a, b, c).midpoint, (a+b+c)/3d)
  }


}
