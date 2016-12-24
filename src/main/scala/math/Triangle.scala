package math

import renderer.Hit
import play.api.libs.json.{Format, JsValue, Json}

final case class Triangle(a: Vector3,
                          b: Vector3,
                          c: Vector3,
                          material: String = "DEFAULT_MATERIAL",
                          normals: Option[Seq[Vector3]] = None)
    extends Shape {
  import Math._
  lazy val edge1: Vector3 = b - a
  lazy val edge2: Vector3 = c - a
  lazy val normal: Vector3 = -(edge1 cross edge2) normalized

  override def intersect(r: Ray): Option[Hit] = {

    //Begin calculating determinant - also used to calculate u parameter
    val p: Vector3 = r.direction cross edge2
    //if determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
    val det: Double = edge1 * p

    //TODO: Backface culling???
    if (det > -EPS && det < EPS)
      None
    else {
      val inv_det = 1f / det
      //calculate distance from V1 to ray origin
      val t_vec = r.origin - a //I normalized this???

      //Calculate u parameter and test bound
      val u = (t_vec * p) * inv_det
      //The intersection lies outside of the triangle
      if (u < 0f || u > 1f)
        None
      else {
        //Prepare to test v parameter
        val q: Vector3 = t_vec cross edge1
        //Calculate V parameter and test bound
        val v = (r.direction * q) * inv_det
        //The intersection lies outside of the triangle
        if (v < 0f || u + v > 1f)
          None
        else {
          val t = (edge2 * q) * inv_det
          if (t > EPS) {
            //ray intersection
            val pos = r.march(t)

            normals match {
              case Some(Seq(a, b, c)) =>
                val interpolatedNormal = a * (1 - u - v) + b * u + c * v
                Some(
                  Hit(t,
                      r.march(t),
                      interpolatedNormal,
                      Shape.getMaterial(material, pos)))
              case _ =>
                Some(
                  Hit(t, r.march(t), normal, Shape.getMaterial(material, pos)))
            }
          } else {
            None
          }
        }
      }
    }
  }

  override def intersect(r: Ray, maxDist: Double): Boolean = {
    val p: Vector3 = r.direction cross edge2
    //if determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
    val det: Double = edge1 * p
    //TODO: Backface culling???
    if (det > -EPS && det < EPS)
      false
    else {
      val inv_det = 1f / det
      //calculate distance from V1 to ray origin
      val t_vec = (r.origin - a)

      //Calculate u parameter and test bound
      val u = (t_vec * p) * inv_det
      //The intersection lies outside of the triangle
      if (u < 0f || u > 1f)
        false
      else {
        //Prepare to test v parameter
        val q: Vector3 = t_vec cross edge1
        //Calculate V parameter and test bound
        val v = (r.direction * q) * inv_det
        //The intersection lies outside of the triangle
        if (v < 0f || u + v > 1f)
          false
        else {
          val t = (edge2 * q) * inv_det
          EPS < t && t < maxDist - EPS
        }
      }
    }
  }

  override def boundingBox: AABB = {
    val points: Seq[Vector3] = Seq(a, b, c)
    AABB(points.map(_.x).min,
         points.map(_.x).max,
         points.map(_.y).min,
         points.map(_.y).max,
         points.map(_.z).min,
         points.map(_.z).max)
  }

  override def midpoint: Vector3 = (a + b + c) / 3

  override def minX: Double = a.x min b.x min c.x

  override def minY: Double = a.y min b.y min c.y

  override def minZ: Double = a.z min b.z min c.z

  override def maxX: Double = a.x max b.x max c.x

  override def maxY: Double = a.y max b.y max c.y

  override def maxZ: Double = a.z max b.z max c.z
}
