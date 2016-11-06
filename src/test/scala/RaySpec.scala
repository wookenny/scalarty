import math.{Ray, Vector3}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import org.specs2.{ScalaCheck, Specification}


class RaySpec  extends Specification with ScalaCheck {


  //TODO separate special cases of reflection and refraction
  def is =
  s2"""
  A ray should
    calculate the direction of a reflection correctly $testReflection
    calculate the direction of a refraction correctly $testRefraction
    calculate the position for a given marching diretion

  """


    val testReflection = forAll { (dir: Vector3, o: Vector3) => {
      val ray = Ray(origin = o, direction = dir.normalized)
      val rray = ray.reflectedAt(o, -ray.direction)
      (rray.direction + ray.direction).length < 0.01 && rray.depth == ray.depth + 1 && ray.n == rray.n
      }
    }

      val testRefraction = forAll { (dir: Vector3, o: Vector3, norm: Vector3) => {
        val ray = Ray(origin = o, direction = dir.normalized, n=1f)
        val refractedRay = ray.refractedAt(position = o, normal = norm, newN = 1f)

        refractedRay match {
          case None => false
          case Some(rray) => (rray.direction - ray.direction).length < 0.01 && rray.depth == ray.depth + 1 && rray.n == 1
        }
      }
  }


  implicit lazy val VectorGen: Arbitrary[Vector3] =
    Arbitrary {
      for{ x : Float <- Gen.choose(-.9f, .9f)
           y : Float <- Gen.choose(-.9f, .9f)
           z : Float <- Gen.choose(-.9f, .9f)
      } yield Vector3(x,y,z)
    }

}