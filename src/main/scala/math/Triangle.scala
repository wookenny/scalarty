package math

import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import renderer.Hit

final case class Triangle(a: VectorBreeze3,
                          b: VectorBreeze3,
                          c: VectorBreeze3,
                          material: String = "DEFAULT_MATERIAL",
                          normals: Option[Seq[VectorBreeze3]] = None)
    extends Shape {
  import Math._
  lazy val edge1: VectorBreeze3 = b - a
  lazy val edge2: VectorBreeze3 = c - a
  lazy val normal: VectorBreeze3 = -(edge1 cross edge2).normalized

  override def intersect(r: Ray): Option[Hit] = {

    //Begin calculating determinant - also used to calculate parameter u
    val p: VectorBreeze3 = r.direction cross edge2
    //if determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
    val det: Double = edge1 dot p


    //TODO: Backface culling???
    if (det > -EPS && det < EPS)
      None
    else {
      val inv_det : Double = 1f / det
      //calculate distance from V1 to ray origin
      val t_vec : VectorBreeze3 = r.origin - a //I normalized this???

      //Calculate u parameter and test bound
      val u = (t_vec dot p) * inv_det
      //The intersection lies outside of the triangle
      if (u < 0f || u > 1f)
        None
      else {
        //Prepare to test v parameter
        val q: VectorBreeze3 = t_vec cross edge1
        //Calculate V parameter and test bound
        val v = (r.direction dot q) * inv_det


        //The intersection lies outside of the triangle
        if (v < 0f || u + v > 1f)
          None
        else {
          val t = (edge2 dot q) * inv_det
          if (t > EPS) {
            //ray intersection
            val pos = r.march(t)

            normals match {
              case Some(Seq(x, y, z)) =>
                val interpolatedNormal = x * (1 - u - v) + y * u + z * v
                Some(Hit(t, r.march(t), interpolatedNormal, Shape.getMaterial(material, pos)))
              case _ =>
                Some(Hit(t, r.march(t), normal, Shape.getMaterial(material, pos)))
            }
          } else {
            None
          }
        }
      }
    }
  }

  override def intersect(r: Ray, maxDist: Double): Boolean = {
    val p: VectorBreeze3 = r.direction cross edge2
    //if determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
    val det: Double = edge1 dot p
    //TODO: Backface culling???
    if (det > -EPS && det < EPS)
      false
    else {
      val inv_det = 1f / det
      //calculate distance from V1 to ray origin
      val t_vec = r.origin - a

      //Calculate u parameter and test bound
      val u = (t_vec dot p) * inv_det
      //The intersection lies outside of the triangle
      if (u < 0f || u > 1f)
        false
      else {
        //Prepare to test v parameter
        val q: VectorBreeze3 = t_vec cross edge1
        //Calculate V parameter and test bound
        val v = (r.direction dot q) * inv_det
        //The intersection lies outside of the triangle
        if (v < 0f || u + v > 1f)
          false
        else {
          val t = (edge2 dot q) * inv_det
          EPS < t && t < maxDist - EPS
        }
      }
    }
  }

  override lazy val boundingBox: AABB = AABB(minX, maxX, minY, maxY, minZ, maxZ)

  override lazy val midpoint: VectorBreeze3 = (a + b + c) / 3d

  override lazy val minX: Double = a(0) min b(0) min c(0)

  override lazy val minY: Double = a(1) min b(1) min c(1)

  override lazy val minZ: Double = a(2) min b(2) min c(2)

  override lazy val maxX: Double = a(0) max b(0) max c(0)

  override lazy val maxY: Double = a(1) max b(1) max c(1)

  override lazy val maxZ: Double = a(2) max b(2) max c(2)
}
