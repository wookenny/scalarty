package bounding

import math.{Ray}
import renderer.Hit


final case class ShapeMetaContainer(shapes: ShapeContainer*) extends ShapeContainer{

  override def size: Int = shapes.foldLeft(0)(_ + _.size)

  override def intersect(r: Ray): Option[Hit] =  shapes.foldLeft(Option.empty[Hit])( (h: Option[Hit],seq: ShapeContainer) => h match {
    case None => seq.intersect(r)
    case Some(hit) => seq.intersect(r) match {
      case Some(otherHit) if otherHit.distance < hit.distance => Some(otherHit)
      case _ => Some(hit)
    }
  })


  override def intersectionTest(r: Ray, maxDist: Double): Boolean = shapes.foldLeft(false)(_ || _.intersectionTest(r,maxDist))
}
