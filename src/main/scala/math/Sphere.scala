package math

import renderer.Hit

final case class Sphere(center: Vector3, radius: Double, material: String = "DEFAULT_MATERIAL")
    extends Shape {
  import Math._
  override def intersect(r: Ray): Option[Hit] = {
    val t_ca: Double = (center - r.origin) * r.direction
    val t_hc2: Double = radius * radius - (center - r.origin) * (center - r.origin) + t_ca * t_ca
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
          lazy val normal = (pos - center).normalized
          Some(Hit(dist, pos, normal, Shape.getMaterial(material, pos)))
        }
        case None => None
      }
    }
  }

  override def intersect(r: Ray, maxDist: Double): Boolean = {
    val t_ca: Double = (center - r.origin) * r.direction
    val t_hc2: Double = radius * radius - (center - r.origin) * (center - r.origin) + t_ca * t_ca
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

  override lazy val boundingBox: AABB =
    AABB(center.x - radius,
         center.x + radius,
         center.y - radius,
         center.y + radius,
         center.z - radius,
         center.z + radius)

  override val midpoint: Vector3 = center

  override lazy val minX = center.x - radius

  override lazy val minY = center.y - radius

  override lazy val minZ = center.z - radius

  override lazy val maxX = center.x + radius

  override lazy val maxY = center.y + radius

  override lazy val maxZ = center.z + radius
}
