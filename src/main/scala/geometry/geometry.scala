package geometry


import Material.{Material, SingleColorMaterial, UnshadedColor}
import play.api.libs.json.{JsValue, Json, OFormat}

object geometry {
  def sqrt(f: Float) = Math.sqrt(f).toFloat

  val EPS: Float = 0.0001.toFloat
}

final case class Vector3(x: Float, y: Float, z: Float){

   def /(s: Float) = Vector3(x/s,y/s,z/s)
   def *(s: Float) = Vector3(s*x,s*y,s*z)


   def +(p:Vector3) = Vector3(x+p.x,y+p.y,z+p.z)
   def -(p:Vector3) = Vector3(x-p.x,y-p.y,z-p.z)
   def *(p: Vector3) = x*p.x + y*p.y + z*p.z

   def unary_-() = this *(-1)
   def unary_+() = this
   def =~=(p: Vector3) = (this.equals(p)) //TODO: implement properly

   def length : Float = geometry.sqrt(this * this)
   def dist(p: Vector3) = (this-p).length
   def normalized =  this/(this.length)
   def cross(p: Vector3) = Vector3( y*p.z - z*p.y,
                                    z*p.x - x*p.z,
                                    x*p.y - y*p.x)

   def pow(exp: Float) = Vector3(Math.pow(x,exp).toFloat, Math.pow(y,exp).toFloat, Math.pow(z,exp).toFloat)
   def expf = Vector3(Math.exp(x).toFloat,Math.exp(y).toFloat,Math.exp(z).toFloat)


}

object Vector3{ //TODO: use Xor

    val ZERO = Vector3(0,0,0)
    val ONE  = Vector3(1,1,1)
    val X    = Vector3(1,0,0)
    val Y    = Vector3(0,1,0)
    val Z    = Vector3(0,0,1)

    implicit val vectorJsonFormat : OFormat[Vector3] = Json.format[Vector3]

    def apply(x: Double, y: Double, z: Double) : Vector3 = apply(x.toFloat, y.toFloat, z.toFloat)

  }

final case class RGB(red: Float, green: Float, blue: Float){
  def ^(pow: Float) = RGB(Math.pow(red,pow).toFloat,
                          Math.pow(green,pow).toFloat,
                          Math.pow(blue,pow).toFloat)

  //TODO :alpha?
  def awtColor() = new java.awt.Color( Math.min(Math.max(0f,red),1f),
                                         Math.min(Math.max(0f,green),1f),
                                         Math.min(Math.max(0f,blue),1f))

  def *(s: Float) = RGB(red*s, green*s, blue*s)
  def /(s: Float) = RGB(red/s, green/s, blue/s)

  def +(c: RGB) = RGB(red+c.red,green+c.green,blue+c.blue)
  def -(c: RGB) = RGB(red-c.red,green-c.green,blue+c.blue)


  def unary_-() = this *(-1)
  def unary_+() = this

  def exposureCorrected = RGB(1-Math.exp(-red).toFloat,
                              1-Math.exp(-green).toFloat,
                              1-Math.exp(-blue).toFloat)
  def gammaCorrected = this ^ (1/RGB.GAMMA)

}

object RGB{

  implicit val colorJsonFormat : OFormat[RGB] = Json.format[RGB]

  val BLACK = RGB(0,0,0)
  val WHITE = RGB(1,1,1)
  val RED   = BLACK.copy(red=1)
  val GREEN = BLACK.copy(green=1)
  val BLUE  = BLACK.copy(blue=1)
  val CYAN  = BLACK.copy(green=1, blue=1)

  val GAMMA = 2.2f

  def apply(r: Double, g: Double, b: Double) : RGB  = apply(r.toFloat, g.toFloat, b.toFloat)
}

final case class Ray(origin: Vector3, direction: Vector3, depth: Int = 0, n : Float = 1){
  import geometry.EPS
  def march(length: Float) = origin + direction * length

  def reflectedAt(position: Vector3, normal: Vector3): Ray = {
    val dir = (direction - normal * (direction * normal) * 2).normalized
    Ray(position + dir*EPS, dir, depth+1, n)
  }

  def refractedAt(position: Vector3, normal: Vector3, newN: Float) = {
    //TODO find nicer and/or faster calculation
    val refractionFactor = n / newN
    val c1 : Float = - normal *  direction
    val c2 : Float = Math.sqrt(1 - refractionFactor*refractionFactor * (1 - c1*c1)).toFloat

    val refractedDir: Vector3 = (direction * n) + normal * (n * c1 - c2)
    Ray(position + refractedDir*EPS, refractedDir, depth+1, newN)
  }
}

final case class Hit(val distance: Float, val position: Vector3, val normal: Vector3, val color: UnshadedColor)
