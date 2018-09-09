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
      calculate interpolated Normals $calculateInterpolatedNormals
      return the minimal axis parallel bounding box $boundingBoxTest
      return its midpoint $midpointTest
  """

  val a = Vector3(-1, -1, 1)
  val b = Vector3(1, -1, 1)
  val c = Vector3(1, 1, 1)

  val triangle = Triangle(a, b, c)

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

  val hittingTest = forAll { r: Ray =>
    triangle.intersect(r).isDefined
  }

  val hittingTestDist = forAll { r: Ray =>
    triangle.intersect(r, 1.1)
  }

  val missingTestDist = forAll { r: Ray =>
    !triangle.intersect(r, .9)
  }

  val missingTest = {
    val miss1 = triangle.intersect(Ray(Vector3(-2, -2, 0), Vector3.Z), 2)
    val miss2 = triangle.intersect(Ray(Vector3(2, 2, 0), Vector3.Z), 2)
    val miss4 = triangle.intersect(Ray(Vector3(-2, -2, 0), Vector3.Z)).isDefined
    val miss5 = triangle.intersect(Ray(Vector3(2, 2, 0), Vector3.Z)).isDefined

    (miss1, miss2) should beEqualTo(false, false)
  }


  val EPS = 0.001
  def approx(x: Double, y: Double): Boolean =
    (x.isNaN && y.isNaN) || Math.abs(x - y) <= EPS
  def approx(a: Vector3, b: Vector3): Boolean =
    approx(a.x, b.x) && approx(a.y, b.y) && approx(a.z, b.z)



  def calculateInterpolatedNormals = forAll { r: Ray =>
    val normalA = Vector3.X
    val normalB = Vector3.Y
    val normalC = Vector3.Z
    val triangleWithNormals = Triangle(a, b, c, normals = Some(Seq(normalA, normalB, normalC)))


    val normal             = triangle.intersect(r).map(_.normal)
    val interpolatedNormal = triangleWithNormals.intersect(r).map(_.normal)



    (normal,interpolatedNormal) match {
      case (Some(n1), Some(n2)) => {
        (n1 ~= -Vector3.Z) &&
          approx(n2.length, 1d) &&
          Seq(n2.x, n2.y, n2.z).min >= 0 //better test for interpolated normals
        }
      case _ => false
    }
  }

  def boundingBoxTest = forAll { (a: Vector3, b: Vector3, c: Vector3) =>
    val aabb = Triangle(a, b, c).boundingBox
    val xs = Seq(a, b, c).map(_.x)
    val ys = Seq(a, b, c).map(_.y)
    val zs = Seq(a, b, c).map(_.z)

    aabb == AABB(xs.min, xs.max, ys.min, ys.max, zs.min, zs.max)
  }

  def midpointTest = forAll { (a: Vector3, b: Vector3, c: Vector3) =>
    Triangle(a, b, c).midpoint ~= a / 3 + b / 3 + c / 3
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
