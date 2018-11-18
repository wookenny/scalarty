package bounding

import math.Ray
import renderer.Hit
import math.Math._

import scala.annotation.tailrec

trait ShapeContainer {
  def size: Int
  def intersect(ray: Ray): Option[Hit]
  def intersectionTest(r: Ray, maxDist: Double): Boolean


  def lightPercentage(r: Ray, maxDist: Double) : Double  = lightPercentage(r,maxDist,1d)

  @tailrec
  private def lightPercentage(r: Ray, maxDist: Double,acc: Double) : Double = intersect(r) match {
    case _ if acc < EPS => acc
    case None => acc
    case Some(hit) if hit.distance >= maxDist => acc
    case Some(hit) => lightPercentage(r.marchedRay(hit.distance+EPS), maxDist-hit.distance,
                                      hit.material.transparency * acc)
  }

}
