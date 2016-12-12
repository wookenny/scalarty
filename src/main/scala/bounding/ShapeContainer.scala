package bounding

import math.Ray
import renderer.Hit

trait ShapeContainer {
  def size : Int
  def intersect(ray: Ray): Option[Hit]
  def intersectionTest(r: Ray, maxDist: Double): Boolean

}
