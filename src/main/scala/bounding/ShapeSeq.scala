package bounding

import math.{Ray, Shape}
import renderer.Hit

final case class ShapeSeq(shapes: Seq[Shape]) extends ShapeContainer {

  override def size: Int = shapes.size

  override def intersect(ray: Ray): Option[Hit] =
    shapes.flatMap { s =>
      s intersect ray
    } reduceOption Ordering.by((_: Hit).distance).min

  override def intersectionTest(r: Ray, maxDist: Double): Boolean =
    shapes.exists { _.intersect(r, maxDist) }
}
