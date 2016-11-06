package math

import renderer.Hit

final case class Sphere(center: Vector3,
                        radius: Float,
                        material: String = "DEFAULT_MATERIAL")
    extends Shape {
  import Math._
  override def intersect(r: Ray): Option[Hit] = {
    val t_ca: Float = (center - r.origin) * r.direction
    val t_hc2: Float = radius * radius - (center - r.origin) * (center - r.origin) + t_ca * t_ca
    if (t_hc2 < 0) //No hit
      None
    else {
      val a = t_ca - sqrt(t_hc2)
      val b = t_ca + sqrt(t_hc2)
      val dist: Option[Float] = (a > EPS, b > EPS) match {
        case (true, true) => Some(min(a, b))
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

  override def intersect(r: Ray, maxDist: Float): Boolean = {
    val t_ca: Float = (center - r.origin) * r.direction
    val t_hc2: Float = radius * radius - (center - r.origin) * (center - r.origin) + t_ca * t_ca
    if (t_hc2 < 0) //No hit
      false
    else {
      val a = t_ca - sqrt(t_hc2)
      if (a > EPS && a < maxDist)
        true
      else {
        val b = t_ca + sqrt(t_hc2)
        if (b > EPS && b < maxDist)
          true
        else false
      }
    }
  }

}
