package math

import renderer.Hit
import play.api.libs.json.{Format, JsValue, Json}


final case class Triangle(a: Vector3, b: Vector3, c: Vector3, material:String = "DEFAULT_MATERIAL") extends  Shape{
  import Math._
  val edge1 : Vector3 = b-a
  val edge2 : Vector3 = c-a
  val normal: Vector3 = -(edge1 cross edge2) normalized

  override def intersect(r: Ray): Option[Hit] = {

    //Begin calculating determinant - also used to calculate u parameter
    val p : Vector3 = r.direction cross edge2
    //if determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
    val det : Float = edge1 * p

    //TODO: Backface culling???
    if(det > -EPS && det < EPS)
      None
    else {
      val inv_det = 1f / det
      //calculate distance from V1 to ray origin
      val t_vec = (r.origin - a) //I normalized this???

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
            Some(Hit(t, r.march(t), normal,  Shape.getMaterial(material,pos)))
          } else {
            None
          }
        }
      }
    }
  }

  override def intersect(r: Ray, maxDist: Float): Boolean = {
    val p : Vector3 = r.direction cross edge2
    //if determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
    val det : Float = edge1 * p
    //TODO: Backface culling???
    if(det > -EPS && det < EPS)
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

}
