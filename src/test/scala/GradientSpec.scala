import color.RGB
import math.Gradient._
import math._
import org.specs2.{ScalaCheck, Specification}
import org.specs2.mock.Mockito
import org.specs2.specification.core.SpecStructure
import math.Gradient.implicits._
import org.scalacheck.Gen
import org.scalacheck.Prop._

class GradientSpec extends Specification with Mockito with ScalaCheck {

  override def is: SpecStructure =
    s2"""
        A Gradient should
          be available for Double ${gradientable[Double]}
          be available for RGB ${gradientable[RGB]}
          be available for Vector3 ${gradientable[Vector3]}
          not be available for String ${!gradientable[String]}

          provide a gradient for one-dimensional functions $testGradient1D
          provide a gradient for two-dimensional functions $testGradient2D
          provide a gradient for three-dimensional functions $testGradient3D
          provide a gradient for four-dimensional functions $testGradient4D
      """


  private case object NoConversion extends (Any => Nothing) {
   def apply(x: Any) = sys.error("No conversion")
  }

  private def noConversion: Any => Nothing = NoConversion

  private def canConvert[A,B](f: A => B) = f ne NoConversion

  private def gradientable[T]()(implicit f: T => Gradientable[T] = NoConversion) = canConvert[T,Gradientable[T]](f)



  val function1dCheckGen = for {
    a <- Gen.choose(-1000d,1000d)
    b <- Gen.choose(-1000d,1000d)
    x <- Gen.choose(-1000d,1000d)
  } yield (a,b,x)

  private def testGradient1D = forAll(function1dCheckGen) { case (a,b,x)  =>
    implicit val f: OneDimensionalFunction[Double] = (x: Double) => a * x + b

    gradient(x) must be closeTo a +/- 0.01
  }

  val function2dCheckGen = for {
    ax <- Gen.choose(-1000d,1000d)
    ay <- Gen.choose(-1000d,1000d)
    b  <- Gen.choose(-1000d,1000d)
    x  <- Gen.choose(-1000d,1000d)
    y  <- Gen.choose(-1000d,1000d)
  } yield (ax,ay,b,x,y)


  private def testGradient2D = forAll(function2dCheckGen) { case (ax,ay,b,x,y)  =>

    implicit val f: TwoDimensionalFunction[Double] = (x: Double, y: Double) => ax*x + ay*y + b

    val (gx,gy) = gradient(x,y)
    (gx must be closeTo ax +/- 0.01) and (gy must be closeTo ay +/- 0.01)
  }

  val function3dCheckGen = for {
    ax <- Gen.choose(-1000d,1000d)
    ay <- Gen.choose(-1000d,1000d)
    az <- Gen.choose(-1000d,1000d)
    b  <- Gen.choose(-1000d,1000d)
    x  <- Gen.choose(-1000d,1000d)
    y  <- Gen.choose(-1000d,1000d)
    z  <- Gen.choose(-1000d,1000d)
  } yield (ax,ay,az,b,x,y,z)


  private def testGradient3D = forAll(function3dCheckGen) { case (ax,ay,az,b,x,y,z)  =>

    implicit val f: ThreeDimensionalFunction[Double] = (x: Double, y: Double, z:Double) => ax*x + ay*y + az*z + b

    val (gx,gy,gz) = gradient(x,y,z)
    (gx must be closeTo ax +/- 0.01) and
      (gy must be closeTo ay +/- 0.01) and
      (gz must be closeTo az +/- 0.01)
  }


  val function4dCheckGen = for {
    ax <- Gen.choose(-1000d,1000d)
    ay <- Gen.choose(-1000d,1000d)
    az <- Gen.choose(-1000d,1000d)
    aw <- Gen.choose(-1000d,1000d)
    b  <- Gen.choose(-1000d,1000d)
    x  <- Gen.choose(-1000d,1000d)
    y  <- Gen.choose(-1000d,1000d)
    z  <- Gen.choose(-1000d,1000d)
    w  <- Gen.choose(-1000d,1000d)
  } yield (ax,ay,az,aw,b,x,y,z,w)


  private def testGradient4D = forAll(function4dCheckGen) { case (ax,ay,az,aw,b,x,y,z,w)  =>

    implicit val f: FourDimensionalFunction[Double] = (x: Double, y: Double, z:Double, w:Double) => ax*x + ay*y + az*z + aw*w + b

    val (gx,gy,gz,gw) = gradient(x,y,z,w)
    (gx must be closeTo ax +/- 0.01) and
      (gy must be closeTo ay +/- 0.01) and
      (gz must be closeTo az +/- 0.01) and
      (gw must be closeTo aw +/- 0.01)
  }

}
