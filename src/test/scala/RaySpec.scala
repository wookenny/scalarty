import geometry.{Ray, Vector3}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import org.specs2.{ScalaCheck, Specification}

/**
  * Created by torsten on 10/16/16.
  */
class RaySpec  extends Specification with ScalaCheck {


  //TODO separate special cases of reflection and refraction
  def is =
  s2"""
  A ray should
    calculate the direction of a reflection correctly $testReflection
    calculate the direction of a refraction correctly
    calculate the position for a given marching diretion

  """


    val testReflection = forAll { (dir: Vector3, o: Vector3) => {
      val ray = Ray(origin = o, direction = dir.normalized)
      val rray = ray.reflectedAt(o + ray.direction, -ray.direction)

      (rray.direction + ray.direction).length < 0.01 && rray.depth == ray.depth + 1 && ray.n == rray.n
      //TODO: test position
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