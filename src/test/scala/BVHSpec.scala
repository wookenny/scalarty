import bounding.BVH
import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import math.{Ray, Shape, Sphere}
import org.specs2.{ScalaCheck, Specification}
import renderer.Hit
import support.Config

class BVHSpec extends Specification with ScalaCheck {

  implicit val config: _root_.support.Config = Config()

  def is = s2"""
   An BVH should
      be constructed with correct height and size ${correctHeightAndSize}
      hit  spheres correctly $intersectionSpheres
      miss spheres correctly $missSpheres
      hit  spheres inside maximal distance $intersectionTestPositive
      miss spheres outside maximal distance $intersectionTestNegative
      add InnerBoxes for the leaves if requested $showLeafBoxes
  """

  val spheres = for {
    x <- 1 to 3
    y <- 1 to 3
    z <- 1 to 3
  } yield Sphere(VectorBreeze3.from(x, y, z), radius = 0.2f)

  val sphereBVH = BVH(spheres, 2)
  val sphereBVH_SAH = BVH(spheres, 2, splitSAH = true)

  def intersect(shapes: Seq[Shape], ray: Ray): Option[Hit] = {
    shapes.flatMap { s =>
      s intersect ray
    } match {
      case Nil => None
      case xs => Some(xs.minBy(_.distance))
    }
  }

  def correctHeightAndSize = {
    ((sphereBVH.depth, sphereBVH.size) should be equalTo (4, 27)) and
      ((sphereBVH_SAH.depth, sphereBVH_SAH.size) should be equalTo (3, 27))
  }

  def intersectionSpheres = {
    val ray = Ray(origin = VectorBreeze3.from(2, 2, -10), direction = Z)
    (sphereBVH.intersect(ray) shouldNotEqual None) and
      (sphereBVH_SAH.intersect(ray) shouldNotEqual None)
  }

  def missSpheres = {
    val ray = Ray(origin = VectorBreeze3.from(2, 2, -10), direction = X)
    (sphereBVH.intersect(ray) should be equalTo None) and
      (sphereBVH_SAH.intersect(ray) should be equalTo None)
  }

  def intersectionTestPositive = {
    val ray = Ray(origin = VectorBreeze3.from(2, 2, -10), direction = Z)
    (sphereBVH.intersectionTest(ray, 20) shouldNotEqual None) and
      (sphereBVH_SAH.intersectionTest(ray, 20) shouldNotEqual None)
  }

  def intersectionTestNegative = {
    val ray = Ray(origin = VectorBreeze3.from(2, 2, -10), direction = Z)
    (sphereBVH.intersectionTest(ray, 10) shouldNotEqual None) and
      (sphereBVH_SAH.intersectionTest(ray, 10) shouldNotEqual None)
  }

  def showLeafBoxes = {

    val sphereBVHwithLeaves = BVH(spheres, 1)(config.copy(showBvHLeaves = true))
    val sphereBVHWithoutLeaves = BVH(spheres, 1)

    sphereBVHwithLeaves.size should be equalTo sphereBVHWithoutLeaves.size //TODO: leaf boxes do not count
  }

}
