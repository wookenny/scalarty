import bounding.{ShapeContainer, ShapeMetaContainer}
import material.Material.DEFAULT_MATERIAL
import math.{Ray, Vector3}
import org.specs2.Specification
import org.specs2.mock.Mockito
import renderer.Hit


class ShapeMetaContainerSpec extends Specification with Mockito{

  override def is = s2"""
    ShapeMetaContainer should
      return the number of all elements for
        an empty meta-container $getSizeForEmptyMetaContainer
        a sequence with only empty container $getSizeForEmptyContainers
        a sequence with some non-empty container $getCorrectSize
      test intersections correctly for
        an empty meta-container $intersectionTestForEmptyMetaContainer
        several containers $intersectionTestForSeveralContainers
      find the closest intersections for
        an empty meta-container $intersectForEmptyMetaContainer
        several containers without a hit $intersectWithoutHit
        several containers with hits $intersectMultipleHits
    """

  val ray = Ray(Vector3.ZERO, Vector3.X)

  val getSizeForEmptyMetaContainer = {
    val metaContainer = ShapeMetaContainer()
    metaContainer.size should be equalTo 0
  }

  val getSizeForEmptyContainers = {
    val container1 = mock[ShapeContainer]
    val container2 = mock[ShapeContainer]
    container1.size returns 0
    container2.size returns 0

    val metaContainer = ShapeMetaContainer(container1, container2)
    metaContainer.size should be equalTo 0
  }

  val getCorrectSize = {
    val container1 = mock[ShapeContainer]
    val container2 = mock[ShapeContainer]
    val container3 = mock[ShapeContainer]
    container1.size returns 12
    container2.size returns 536
    container3.size returns 1043

    val metaContainer = ShapeMetaContainer(container1, container2, container3)
    metaContainer.size should be equalTo container1.size+container2.size+container3.size
  }

  val intersectionTestForEmptyMetaContainer = {
    val metaContainer = ShapeMetaContainer()
    metaContainer.intersectionTest(ray, Double.MaxValue) should be equalTo false
  }

  val intersectionTestForSeveralContainers = {
    val (shortDist,middleDist,longDist) = (1d,2d,3d)
    val container1 = mock[ShapeContainer]
    val container2 = mock[ShapeContainer]
    val container3 = mock[ShapeContainer]

    for(dist: Double <- Seq(Double.MaxValue,shortDist,middleDist,longDist)){
      container1.intersectionTest(ray,dist) returns false
      container2.intersectionTest(ray,dist) returns (dist > middleDist)
      container3.intersectionTest(ray,dist) returns (dist > shortDist)
    }

    val metaContainer = ShapeMetaContainer(container1, container2, container3)

    (metaContainer.intersectionTest(ray, Double.MaxValue) should be equalTo true) and
      (metaContainer.intersectionTest(ray,longDist)   should be equalTo true)   and
      (metaContainer.intersectionTest(ray,middleDist) should be equalTo true)  and
      (metaContainer.intersectionTest(ray,shortDist)  should be equalTo false) and
      (metaContainer.intersectionTest(ray,0d)  should be equalTo false)
  }


  val intersectForEmptyMetaContainer = {
    val metaContainer = ShapeMetaContainer()
    metaContainer.intersect(ray) should be equalTo None
  }

  val intersectWithoutHit = {

    val container1 = mock[ShapeContainer]
    val container2 = mock[ShapeContainer]
    val container3 = mock[ShapeContainer]

    container1.intersect(ray) returns None
    container2.intersect(ray) returns None
    container3.intersect(ray) returns None

    val metaContainer = ShapeMetaContainer(container1, container2, container3)
    metaContainer.intersect(ray) should be equalTo None
  }

  val intersectMultipleHits = {
    val container1 = mock[ShapeContainer]
    val container2 = mock[ShapeContainer]
    val container3 = mock[ShapeContainer]

    val closestHit = Hit(1,Vector3.ZERO,Vector3.X,DEFAULT_MATERIAL.getMat(Vector3.ZERO))
    container1.intersect(ray) returns Some(closestHit.copy(distance=2))
    container2.intersect(ray) returns Some(closestHit)
    container3.intersect(ray) returns Some(closestHit.copy(distance=3))

    val metaContainer = ShapeMetaContainer(container1, container2, container3)
    metaContainer.intersect(ray) should be equalTo Some(closestHit)
  }

}
