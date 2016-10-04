import geometry.{Ray, Triangle, Vector3}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}


class TriangleSpec  extends Specification with ScalaCheck { def is = s2"""
   A triangle should
      be intersected frontally ${intersectFrontal}
      be intersected frontally correctly {hittingTest.verbose}
      be intersected frontally with correct dist {hittingTestDist.verbose}
      be missed frontally with correct dist {hittingTestDist.verbose}
  """

  def a = Vector3(-1,-1, 1)
  def b = Vector3( 1,-1, 1)
  def c = Vector3(-1, 1, 1)

  def triangle = Triangle(a,b,c)

  def intersectFrontal = {

    val hit = triangle.intersect( Ray( Vector3(-0.2f,-0.2f,0.0f), Vector3.Z))
    hit shouldNotEqual None
  }

  val hittingTest      = forAll { (r: Ray) =>  triangle.intersect(r) != None }
  val hittingTestDist  = forAll { (r: Ray) =>  triangle.intersect(r, 1.1f) }
  val missingTestDist  = forAll { (r: Ray) =>  !triangle.intersect(r, .9f) }

  implicit lazy val HittingRayGen: Arbitrary[Ray] =
    Arbitrary {
      for{ x : Float <- Gen.choose(-.9f,.9f)
           y : Float <- Gen.choose(-.9f, .9f-x)
      } yield Ray(Vector3(x,y,0) , Vector3.Z)
    }




}
