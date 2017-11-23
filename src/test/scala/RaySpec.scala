import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import math.Ray
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import org.specs2.{ScalaCheck, Specification}

class RaySpec extends Specification with ScalaCheck {

  //TODO separate special cases of reflection and refraction
  def is =
    s2"""
  A ray should
    calculate the direction of a reflection correctly $testReflection
    calculate the direction of a refraction correctly $testRefraction
    calculate the position for a given marching diretion $pending

  """

  val testReflection = forAll { (dir: VectorBreeze3, o: VectorBreeze3) =>
    {
      val ray = Ray(origin = o, direction = dir.normalized)
      val rray = ray.reflectedAt(o, -ray.direction)
      (rray.direction ~= -ray.direction) && rray.depth == ray.depth + 1 && ray.n == rray.n
    }
  }

  val testRefraction = forAll { (dir: VectorBreeze3, o: VectorBreeze3, norm: VectorBreeze3) =>
    {
      val ray = Ray(origin = o, direction = dir.normalized)
      val refractedRay =
        ray.refractedAt(position = o, normal = norm, newN = 1f)

      refractedRay match {
        case None => false
        case Some(rray) =>
          (rray.direction ~= ray.direction) && rray.depth == ray.depth + 1 && rray.n == 1
      }
    }
  }

  implicit lazy val VectorGen: Arbitrary[VectorBreeze3] =
    Arbitrary {
      for {
        x: Double <- Gen.choose(-.9, .9)
        y: Double <- Gen.choose(-.9, .9)
        z: Double <- Gen.choose(-.9, .9)
      } yield VectorBreeze3(x, y, z)
    }

}
