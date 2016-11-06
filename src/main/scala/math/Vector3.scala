package math

import Math._
import play.api.libs.json.{Format, Json}


final case class Vector3(x: Float, y: Float, z: Float){

   def /(s: Float) = Vector3(x/s,y/s,z/s)
   def *(s: Float) = Vector3(s*x,s*y,s*z)


   def +(p:Vector3) = Vector3(x+p.x,y+p.y,z+p.z)
   def -(p:Vector3) = Vector3(x-p.x,y-p.y,z-p.z)
   def *(p: Vector3) = x*p.x + y*p.y + z*p.z

   def unary_-() = this *(-1)
   def unary_+() = this
   def =~=(p: Vector3) = (this.equals(p)) //TODO: implement properly

   def length : Float = sqrt(this * this)
   def dist(p: Vector3) = (this-p).length
   def normalized =  this/(this.length)
   def cross(p: Vector3) = Vector3( y*p.z - z*p.y,
                                    z*p.x - x*p.z,
                                    x*p.y - y*p.x)

   def pow(exp: Float) = Vector3(scala.math.pow(x,exp).toFloat, scala.math.pow(y,exp).toFloat,scala.math.pow(z,exp).toFloat)
   def expf = Vector3(scala.math.exp(x).toFloat,scala.math.exp(y).toFloat,scala.math.exp(z).toFloat)


}

object Vector3{ //TODO: use Xor

    val ZERO = Vector3(0,0,0)
    val ONE  = Vector3(1,1,1)
    val X    = Vector3(1,0,0)
    val Y    = Vector3(0,1,0)
    val Z    = Vector3(0,0,1)

    implicit val vectorJsonFormat : Format[Vector3] = Json.format[Vector3]

    def apply(x: Double, y: Double, z: Double) : Vector3 = apply(x.toFloat, y.toFloat, z.toFloat)

  }