import geometry.geometry.Vector3
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.specs2.{ScalaCheck, Specification}




class PointSpec extends Specification with ScalaCheck { def is = s2"""
  A Point should
    have correct length ${lengthTest}
    add correctly ${pointAddition}
    add s$symmetricAddition
  """

  val lengthTest  = forAll { (p: Vector3) => p.length == Math.sqrt(p.x*p.x+p.y*p.y+p.z*p.z).toFloat }
  val pointAddition = forAll { (p: Vector3, q: Vector3) =>  q+p == Vector3(p.x+q.x,p.y+q.y,p.z+q.z) }
  val symmetricAddition = forAll { (p: Vector3, q: Vector3) =>  q+p == p+q }

  implicit lazy val PointGen: Arbitrary[Vector3] =
    Arbitrary {
      for{ x : Float <- Gen.choose(Float.MinValue,Float.MaxValue)
           y : Float <- Gen.choose(Float.MinValue,Float.MaxValue)
           z : Float <- Gen.choose(Float.MinValue,Float.MaxValue)
      } yield Vector3(x,y,z)
    }
}