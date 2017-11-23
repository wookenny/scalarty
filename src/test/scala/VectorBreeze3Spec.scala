import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

class VectorBreeze3Spec extends Specification with ScalaCheck {
  def is = s2"""
  A Vector should
    calculate its length ${lengthTest}
    calculate the distance to another vector ${distTest}
    normalize correctly ${normalizedTest}

    have correct scalar division ${scalarDivision}
    have a correct scalar multiplication $scalarMultiplication
    have a correct negation $negation
    add two vectors $addition
    multiply two vectors $multiplication
    calculate the cross product of two vectors $crossproduct

  """

  val lengthTest = forAll { (p: VectorBreeze3) =>
    approx(p.length, Math.sqrt(p(0)*p(0) + p(1)*p(1) + p(2)*p(2)))
  }

  val distTest = forAll { (a: VectorBreeze3, b: VectorBreeze3) =>
    approx(
      (a-b).length,
      Math.sqrt((a(0)- b(0)) * (a(0)- b(0)) + (a(1) - b(1)) * (a(1) - b(1)) + (a(2) - b(2)) * (a(2) - b(2))))
  }
  val normalizedTest = forAll { (p: VectorBreeze3) =>
    approx(1d, p.normalized.length)
  }

  val scalarDivision = forAll { (a: VectorBreeze3, s: Double) =>
    a/s ~= VectorBreeze3(a(0)/s, a(1)/s, a(2)/s)
  }
  val scalarMultiplication = forAll { (a: VectorBreeze3, s: Double) =>
    a * s ~= VectorBreeze3(a(0)* s, a(1) * s, a(2) * s)
  }
  val negation = forAll { (a: VectorBreeze3) =>
    (-a ~= VectorBreeze3(-a(0), -a(1), -a(2))) && (-(-a) ~= a)
  }

  val addition = forAll { (p: VectorBreeze3, q: VectorBreeze3) =>
    (q + p) ~= VectorBreeze3(p(0)+ q(0), p(1) + q(1), p(2) + q(2))
  }
  val multiplication = forAll { (a: VectorBreeze3, b: VectorBreeze3) =>
    approx( a dot b , a(0) * b(0) + a(1) * b(1) + a(2) * b(2))
  }
  val crossproduct = forAll { (a: VectorBreeze3, b: VectorBreeze3) =>
    ((a cross a) ~= VectorBreeze3.ZERO) && ((a cross b) ~= -(b cross a))
  }
/*
  val powTest = forAll { (a: VectorBreeze3, f: Double) =>
    approx(a.pow(f), VectorBreeze3.from(Math.pow(a(0), f), Math.pow(a(1), f), Math.pow(a(2), f)))
  }

  val expTest = forAll { (a: VectorBreeze3) =>
    a.expf == VectorBreeze3.from(Math.exp(a(0)), Math.exp(a(1)), Math.exp(a(2)))
  }
*/
  implicit lazy val VectorGen: Arbitrary[VectorBreeze3] =
    Arbitrary {
      for {
        x: Double <- Gen.choose(-1000d, 1000d)
        y: Double <- Gen.choose(-1000d, 1000d)
        z: Double <- Gen.choose(-1000d, 1000d)
      } yield VectorBreeze3(x, y, z)
    }

  implicit lazy val DoubleGen: Arbitrary[Double] = Arbitrary {
    Gen.choose(-10d, 10d)
  }

  val EPS = 0.001
  def approx(x: Double, y: Double): Boolean =
    (x.isNaN && y.isNaN) || Math.abs(x - y) <= EPS

}
