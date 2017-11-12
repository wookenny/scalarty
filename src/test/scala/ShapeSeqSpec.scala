import bounding.ShapeSeq
import material.Material.DEFAULT_MATERIAL
import math.breeze.VectorBreeze3._
import math.{Ray, Shape}
import org.specs2.Specification
import org.specs2.mock.Mockito
import renderer.Hit

class ShapeSeqSpec extends Specification with Mockito {

  override def is = s2"""
       A ShapeSeq should
            return the correct size for an empty Seq $testSizeForEmptySeq
            return the correct size for a non-empty Seq $testSizeForNonEmptySeq
          find he closest intersection for
            an empty Seq $intersectForNonEmptySeq
          test for intersection correctly for
            an empty Seq $intersectionTestForEmptySeq
            a non-empty Seq $intersectionTestForNonEmptySeq
    """

  val ray = Ray(ZERO, X)
  val emptyShapeSeq = ShapeSeq(Seq.empty[Shape])

  val testSizeForEmptySeq = emptyShapeSeq.size should be equalTo 0

  val testSizeForNonEmptySeq = {
    val shapes = mock[Seq[Shape]]
    shapes.size returns 2431

    ShapeSeq(shapes).size should be equalTo shapes.size
  }

  val intersectForEmptySeq = emptyShapeSeq.intersect(ray) should be equalTo None

  val intersectForNonEmptySeq = {
    val closestHit =
      Hit(1, ZERO, X, DEFAULT_MATERIAL.getMat(ZERO))

    val shape1 = mock[Shape]
    val shape2 = mock[Shape]
    val shape3 = mock[Shape]
    val (dist1, dist2) = (1, 2)

    shape1.intersect(ray) returns None
    shape2.intersect(ray) returns Some(closestHit.copy(distance = 3))
    shape3.intersect(ray) returns Some(closestHit)

    (ShapeSeq(Seq(shape1, shape1)).intersect(ray) should be equalTo None) and
      (ShapeSeq(Seq(shape1, shape2, shape3))
        .intersect(ray) should be equalTo Some(closestHit))
  }

  val intersectionTestForEmptySeq = emptyShapeSeq.intersectionTest(ray, Double.MaxValue) should be equalTo false

  val intersectionTestForNonEmptySeq = {
    val shape1 = mock[Shape]
    val shape2 = mock[Shape]
    val (dist1, dist2) = (1, 2)

    shape1.intersect(ray, dist1) returns false
    shape1.intersect(ray, dist2) returns false

    shape2.intersect(ray, dist1) returns false
    shape2.intersect(ray, dist2) returns true

    val shapeSeq = ShapeSeq(Seq(shape1, shape2))

    (shapeSeq.intersectionTest(ray, dist1) should be equalTo false) and
      (shapeSeq.intersectionTest(ray, dist2) should be equalTo true)
  }
}
