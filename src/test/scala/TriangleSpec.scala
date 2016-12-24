import math.{Ray, Triangle, Vector3}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

class TriangleSpec extends Specification with ScalaCheck {
  def is = s2"""
   A triangle should
      be intersected from both sides $intersectFrontal
      be intersected accordingly to distance $intersectAccordingToDistance
      be intersected frontally correctly ${hittingTest.verbose}
      be intersected frontally with large enough distance {hittingTestDist.verbose}
      be missed frontally due to short distance {hittingTestDist.verbose}
  """
//TODO: fix the three broken tests
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

    (hitFront, hitBack, missFront, missBack) should beEqualTo(
      (true, true, false, false))
  }

  val hittingTest = forAll { (r: Ray) =>
    triangle.intersect(r).isDefined
  } //FAILS
  val hittingTestDist = forAll { (r: Ray) =>
    triangle.intersect(r, 1.1)
  } //FAILS
  val missingTestDist = forAll { (r: Ray) =>
    !triangle.intersect(r, .9)
  } //FAILS

  implicit lazy val HittingRayGen: Arbitrary[Ray] =
    Arbitrary {
      for {
        x: Double <- Gen.choose(-.9, .9)
        y: Double <- Gen.choose(-.9, x)
      } yield Ray(Vector3(x, y, 0), Vector3.Z)
    }

}
