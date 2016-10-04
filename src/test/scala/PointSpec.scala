import geometry.Vector3
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.specs2.{ScalaCheck, Specification}




class VectorSpec extends Specification with ScalaCheck { def is = s2"""
  A Vector should
    calculate its length ${lengthTest}
    calculate the distance to another vector ${distTest}
    normalize correctly ${normalizedTest}

    have correct scalar division ${scalarDivision}
    have a correct scalar multiplication $scalarMultiplication
    have a correct nagation $negation
    have a correct identiy $identity
    add two vectors $addition
    multiply two vectors $multiplication
    calculate the cross product of two vectors $crossproduct
    calculate the power for a vector $powTest
    calculate the exp function of a vector $expTest
  """


  val lengthTest  = forAll { (p: Vector3) => p.length == Math.sqrt(p.x*p.x+p.y*p.y+p.z*p.z).toFloat }
  val distTest    = forAll { (a: Vector3, b: Vector3) =>  approx(a.dist(b), Math.sqrt( (a.x-b.x)*(a.x-b.x) + (a.y-b.y)*(a.y-b.y) +(a.z-b.z)*(a.z-b.z))) }
  val normalizedTest  = forAll { (p: Vector3) => approx(1, p.normalized.length)  }

  val scalarDivision =  forAll { (a: Vector3, s: Float) => a/s == Vector3(a.x/s, a.y/s, a.z/s ) }
  val scalarMultiplication =  forAll { (a: Vector3, s: Float) => a*s == Vector3(a.x*s, a.y*s, a.z*s ) }
  val negation =  forAll { (a: Vector3) => -a == Vector3(-a.x, -a.y, -a.z ) && -(-a)==a }
  val identity =  forAll { (a: Vector3) => +a == a}

  val addition = forAll { (p: Vector3, q: Vector3) =>  q+p == Vector3(p.x+q.x,p.y+q.y,p.z+q.z) }
  val multiplication = forAll { (a: Vector3, b: Vector3) =>  a*b == a.x*b.x + a.y*b.y + a.z*b.z }
  val crossproduct = forAll { (a: Vector3, b: Vector3) =>  (a cross a) == Vector3.ZERO   && ((a cross b) == -(b cross a)) }

  val powTest = forAll { (a: Vector3, f: Float) => approx( a.pow(f) , Vector3(Math.pow(a.x,f).toFloat, Math.pow(a.y,f).toFloat, Math.pow(a.z,f).toFloat))}

  val expTest = forAll { (a: Vector3) => a.expf == Vector3(Math.exp(a.x).toFloat, Math.exp(a.y).toFloat, Math.exp(a.z).toFloat) }

  implicit lazy val PointGen: Arbitrary[Vector3] =
    Arbitrary {
      for{ x : Float <- Gen.choose(-1000f,1000f)
           y : Float <- Gen.choose(-1000f,1000f)
           z : Float <- Gen.choose(-1000f,1000f)
      } yield Vector3(x,y,z)
    }

  implicit lazy val FloatGen: Arbitrary[Float] = Arbitrary { Gen.choose(-10f,10f)}


  val EPS = 0.001
  def approx(x: Double, y: Double): Boolean  = (x.isNaN && y.isNaN) || Math.abs(x-y)<=EPS
  def approx(a:Vector3, b: Vector3): Boolean  = approx(a.x,b.x) &&  approx(a.y,b.y) &&  approx(a.z,b.z)
}


