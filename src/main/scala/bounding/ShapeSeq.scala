package bounding

import math.{Ray, Shape}
import renderer.Hit

final case class ShapeSeq(shapes: Seq[Shape]) extends ShapeContainer{

  override def size: Int = shapes.size

  override def intersect(ray: Ray): Option[Hit] = shapes.flatMap { s =>
    s intersect ray
  } match {
    case Nil => None
    case xs => Some(xs.minBy(_.distance))
  }

  override def intersectionTest(r: Ray, maxDist: Double): Boolean = shapes.exists { s =>
    s.intersect(r, maxDist)
  }
}
