package math

import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import renderer.Hit

final case class Sphere(center: VectorBreeze3, radius: Double, material: String = "DEFAULT_MATERIAL")
    extends Shape {
  import Math._
  override def intersect(r: Ray): Option[Hit] = {


    val t_ca: Double = (center - r.origin) dot r.direction
    val t_hc2: Double = radius * radius - ((center - r.origin) dot (center - r.origin)) + t_ca * t_ca
    if (t_hc2 < 0) //No hit
      None
    else {
      val a = t_ca - scala.math.sqrt(t_hc2)
      val b = t_ca + scala.math.sqrt(t_hc2)
      val dist: Option[Double] = (a > EPS, b > EPS) match {
        case (true, true) => Some(a min b)
        case (true, _) => Some(a)
        case (_, true) => Some(b)
        case _ => None
      }

      dist match {
        case Some(dist) => {
          lazy val pos = r.march(dist)
          lazy val normal = normalized(pos - center)
          Some(Hit(dist, pos, normal, Shape.getMaterial(material, pos)))
        }
        case None => None
      }
    }
  }

  override def intersect(r: Ray, maxDist: Double): Boolean = {
    val t_ca: Double = (center - r.origin) dot r.direction
    val t_hc2: Double = radius * radius - ((center - r.origin) dot (center - r.origin)) + t_ca * t_ca
    if (t_hc2 < 0) //No hit
      false
    else {
      val a = t_ca - scala.math.sqrt(t_hc2)
      if (a > EPS && a < maxDist)
        true
      else {
        val b = t_ca + scala.math.sqrt(t_hc2)
        if (b > EPS && b < maxDist)
          true
        else false
      }
    }
  }

  override def boundingBox(): AABB =
    AABB(center(0) - radius,
         center(0) + radius,
         center(1) - radius,
         center(1) + radius,
         center(2) - radius,
         center(2) + radius)

  override def midpoint: VectorBreeze3 = center

  override def minX = center(0) - radius

  override def minY = center(1) - radius

  override def minZ = center(2) - radius

  override def maxX = center(0) + radius

  override def maxY = center(1) + radius

  override def maxZ = center(2) + radius
}
