package geometry

import java.awt.Color
import Material.{Material, SingleColorMaterial, UnshadedColor}

object geometry {
  def sqrt(f: Float) = Math.sqrt(f).toFloat

  val EPS: Float = 0.0001.toFloat
}

case class Vector3(val x: Float, val y: Float, val z: Float){

   def /(s: Float) = Vector3(x/s,y/s,z/s)
   def *(s: Float) = Vector3(s*x,s*y,s*z)


   def +(p:Vector3) = Vector3(x+p.x,y+p.y,z+p.z)
   def -(p:Vector3) = Vector3(x-p.x,y-p.y,z-p.z)
   def *(p: Vector3) = x*p.x + y*p.y + z*p.z

   def unary_-() = this *(-1)
   def unary_+() = this

   def length : Float = geometry.sqrt(this * this)
   def dist(p: Vector3) = (this-p).length
   def normalized =  this/(this.length)
   def cross(p: Vector3) = Vector3( y*p.z - z*p.y,
                                    z*p.x - x*p.z,
                                    x*p.y - y*p.x)

   def pow(exp: Float) = Vector3(Math.pow(x,exp).toFloat, Math.pow(y,exp).toFloat, Math.pow(z,exp).toFloat)
   def expf = Vector3(Math.exp(x).toFloat,Math.exp(y).toFloat,Math.exp(z).toFloat)
}

  object Vector3{
    val ZERO = Vector3(0,0,0)
    val ONE  = Vector3(1,1,1)
    val X    = Vector3(1,0,0)
    val Y    = Vector3(0,1,0)
    val Z    = Vector3(0,0,1)
  }


case class Ray(val origin: Vector3, val direction: Vector3){
  def march(length: Float) = origin + direction * length
}

case class Hit(val distance: Float, val position: Vector3, val normal: Vector3, val color: UnshadedColor)

trait Shape{
   var material: Material = SingleColorMaterial(Color.WHITE,0.1f,0.7f,0.2f)
   def intersect(r: Ray) : Option[Hit] //TODO: should only generate needed data, not too much in advance
   def intersect(r: Ray,maxDist: Float) : Boolean


}

  case class Sphere(val center: Vector3, val radius: Float) extends  Shape{
    import geometry._
    override def intersect(r: Ray): Option[Hit] = {
      val t_ca : Float  = (center-r.origin) * r.direction
      val t_hc2 : Float = radius*radius - (center-r.origin)*(center-r.origin) + t_ca*t_ca
      if(t_hc2 < 0) //No hit
        return None

      val a = t_ca - sqrt(t_hc2)
      val b = t_ca + sqrt(t_hc2)
      val dist : Option[Float] = (a >EPS,b > EPS) match {
        case (true,true)  => Some(Math.min(a,b))
        case (true,_)     => Some(a)
        case (_,true)     => Some(b)
        case _ => None
      }

      dist match {
        case Some(dist) => {  lazy val pos = r.march(dist)
          lazy val normal = (pos - center).normalized
          Some( Hit(dist, pos, normal, material.getMat(pos)))}
        case None => None
      }

    }

    //TODO: should only generate needed data, not too much in advance
    override def intersect(r: Ray, maxDist: Float): Boolean = {
      val t_ca : Float  = (center-r.origin) * r.direction
      val t_hc2 : Float = radius*radius - (center-r.origin)*(center-r.origin) + t_ca*t_ca
      if(t_hc2 < 0) //No hit
        return false

      val a = t_ca - sqrt(t_hc2)
      if (a>EPS && a<maxDist)
        return true

      val b = t_ca + sqrt(t_hc2)
      if (b>EPS && b<maxDist)
        return true
       return false
    }
  }


  case class AABB(val x_min: Float, val x_max : Float,
                  val y_min: Float, val y_max : Float,
                  val z_min: Float, val z_max : Float) extends  Shape {
    import geometry._
    require(x_min < x_max)
    require(y_min < y_max)
    require(z_min < z_max)

    override def intersect(r: Ray): Option[Hit] = {

      val dirfrac = Vector3(1 / r.direction.x, 1 / r.direction.y, 1 / r.direction.z)

      val t1 = (x_min - r.origin.x) * dirfrac.x
      val t2 = (x_max - r.origin.x) * dirfrac.x

      val t3 = (y_min - r.origin.y) * dirfrac.y
      val t4 = (y_max - r.origin.y) * dirfrac.y

      val t5 = (z_min - r.origin.z) * dirfrac.z
      val t6 = (z_max - r.origin.z) * dirfrac.z


      val distances = Seq((t1, t2), (t3, t4), (t5, t6))
      val (tmin, normal) = distances.zip(Seq(Vector3.X, Vector3.Y, Vector3.Z))
        .map { case ((x, y), n) => if (x < y) (x, n * (-1)) else (y, n) }
        .maxBy(_._1)
      val tmax: Float = distances.map { case (x, y) => Math.max(x, y) }.min

      if (tmax < EPS || tmin > tmax) {
        None
      } else {
        //use tmax if we are inside the AABB
        val t = if (tmin < 0) tmax else tmin
        val pos = r.march(t)
        Some(Hit(t, pos, normal, material.getMat(pos)))
      }
    }

    //TODO: should only generate needed data, not too much in advance
    override def intersect(r: Ray, maxDist: Float): Boolean = {
      val dirfrac = Vector3(1 / r.direction.x, 1 / r.direction.y, 1 / r.direction.z)

      val t1 = (x_min - r.origin.x) * dirfrac.x
      val t2 = (x_max - r.origin.x) * dirfrac.x

      val t3 = (y_min - r.origin.y) * dirfrac.y
      val t4 = (y_max - r.origin.y) * dirfrac.y

      val t5 = (z_min - r.origin.z) * dirfrac.z
      val t6 = (z_max - r.origin.z) * dirfrac.z


      val distances = Seq((t1, t2), (t3, t4), (t5, t6))
      val tmin = distances.map { case (x, y) => Math.min(x, y) }.max
      val tmax: Float = distances.map { case (x, y) => Math.max(x, y) }.min
      if (tmax < EPS || tmin > tmax )
        return false
      return tmin < maxDist
    }
  }




case class Triangle(val a: Vector3, val b: Vector3, val c: Vector3) extends  Shape{
  import geometry._
  val edge1 : Vector3 = b-a
  val edge2 : Vector3 = c-a
  val normal: Vector3 = -(edge1 cross edge2) normalized

  override def intersect(r: Ray): Option[Hit] = {

      //Begin calculating determinant - also used to calculate u parameter
      val p : Vector3 = r.direction cross edge2
      //if determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
      val det : Float = edge1 * p
      //NOT CULLING
      if(det > -EPS && det < EPS)
        return None
      val inv_det = 1f / det
      //calculate distance from V1 to ray origin
      val t_vec  = (r.origin - a) //I normalized this???

      //Calculate u parameter and test bound
      val u = (t_vec * p)  * inv_det
      //The intersection lies outside of the triangle
      if(u < 0f || u > 1f)
        return None

      //Prepare to test v parameter
      val q : Vector3 =  t_vec cross edge1
      //Calculate V parameter and test bound
      val v = (r.direction * q) * inv_det
      //The intersection lies outside of the triangle
      if(v < 0f || u + v  > 1f)
        return None

      val t = (edge2 * q) * inv_det
      if(t > EPS) { //ray intersection
        val pos = r.march(t)
        Some(Hit(t, r.march(t), normal, material.getMat(pos)))
      }else{
        None
      }
  }

  override def intersect(r: Ray, maxDist: Float): Boolean = {
    val p : Vector3 = r.direction cross edge2
    //if determinant is near zero, ray lies in plane of triangle or ray is parallel to plane of triangle
    val det : Float = edge1 * p
    //NOT CULLING
    if(det > -EPS && det < EPS)
      return false
    val inv_det = 1f / det
    //calculate distance from V1 to ray origin
    val t_vec  = (r.origin - a)

    //Calculate u parameter and test bound
    val u = (t_vec * p)  * inv_det
    //The intersection lies outside of the triangle
    if(u < 0f || u > 1f)
      return false

    //Prepare to test v parameter
    val q : Vector3 =  t_vec cross edge1
    //Calculate V parameter and test bound
    val v = (r.direction * q) * inv_det
    //The intersection lies outside of the triangle
    if(v < 0f || u + v  > 1f)
      return false

    return (edge2 * q) * inv_det > EPS
   }

}