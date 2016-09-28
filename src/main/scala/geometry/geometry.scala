package geometry

import java.awt.Color
import Material.{Material, SingleColorMaterial, UnshadedColor}

object geometry {
  type Real = Float

  def sqrt(r: Real) = Math.sqrt(r).toFloat

  val EPS: Real = 0.0001.toFloat


case class Vector3(val x: Real, val y: Real, val z: Real){ //TODO array? other type?

   def /(s: Real) = Vector3(x/s,y/s,z/s)
   def /(s: Int) = Vector3(x/s,y/s,z/s)
   def *(s: Real) = Vector3(s*x,s*y,s*z)


   def +(p:Vector3) = Vector3(x+p.x,y+p.y,z+p.z)
   def -(p:Vector3) = Vector3(x-p.x,y-p.y,z-p.z)
   def *(p: Vector3) = x*p.x+y*p.y+z*p.z

   def unary_-() = this *(-1)
   def unary_+() = this

   def length : Real = sqrt(this * this)
   def dist(p: Vector3) = (this-p).length
   def normalized =  this/(this.length)
   def cross(p: Vector3) = Vector3(y*p.z-z*p.y, z*p.x-x*p.z, x*p.y-y-p.x)

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

  case class Sphere(val center: Vector3, val radius: Real) extends  Shape{
    override def intersect(r: Ray): Option[Hit] = {
      val t_ca : Real  = (center-r.origin) * r.direction
      val t_hc2 : Real = radius*radius - (center-r.origin)*(center-r.origin) + t_ca*t_ca
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
    override def intersect(r: Ray, maxDist: Real): Boolean = {
      val t_ca : Real  = (center-r.origin) * r.direction
      val t_hc2 : Real = radius*radius - (center-r.origin)*(center-r.origin) + t_ca*t_ca
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
    override def intersect(r: Ray, maxDist: Real): Boolean = {
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
}


/***
  some code for AABB intersetion:

  // r.dir is unit direction vector of ray
dirfrac.x = 1.0f / r.dir.x;
dirfrac.y = 1.0f / r.dir.y;
dirfrac.z = 1.0f / r.dir.z;
// lb is the corner of AABB with minimal coordinates - left bottom, rt is maximal corner
// r.org is origin of ray
float t1 = (lb.x - r.org.x)*dirfrac.x;
float t2 = (rt.x - r.org.x)*dirfrac.x;
float t3 = (lb.y - r.org.y)*dirfrac.y;
float t4 = (rt.y - r.org.y)*dirfrac.y;
float t5 = (lb.z - r.org.z)*dirfrac.z;
float t6 = (rt.z - r.org.z)*dirfrac.z;

float tmin = max(max(min(t1, t2), min(t3, t4)), min(t5, t6));
float tmax = min(min(max(t1, t2), max(t3, t4)), max(t5, t6));

// if tmax < 0, ray (line) is intersecting AABB, but whole AABB is behing us
if (tmax < 0)
{
    t = tmax;
    return false;
}

// if tmin > tmax, ray doesn't intersect AABB
if (tmin > tmax)
{
    t = tmax;
    return false;
}

t = tmin;
return true;

  **/