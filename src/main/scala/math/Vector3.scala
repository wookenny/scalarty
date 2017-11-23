package math

import play.api.libs.json._
import play.api.libs.functional.syntax._
import _root_.breeze.linalg._
import play.api.libs.json.Reads._

object breeze{
   case class VectorBreeze3(v: DenseVector[Double]) {
     require(v.length == 3)

     lazy val length: Double = scala.math.sqrt(v dot v)
     lazy val normalized  = VectorBreeze3(v/length)
     def cross(v2: VectorBreeze3) : VectorBreeze3 = VectorBreeze3(v(1) * v2(2) - v(2) * v2(1), v(2) * v2(0) - v(0) * v2(2), v(0) * v2(1) - v(1) * v2(0))

     lazy val inverse : VectorBreeze3 = VectorBreeze3(1d/v)
     def dot(v2: VectorBreeze3): Double = v dot v2.v

     def apply(i:Int) = v.apply(i)

     def ~=(v2: VectorBreeze3, delta: Double = 0.001): Boolean = (this-v2).length < delta

     def +(v2: VectorBreeze3) = VectorBreeze3(v+v2.v)
     def -(v2: VectorBreeze3) = VectorBreeze3(v-v2.v)
     def /(s:Double) =  VectorBreeze3(v/s)
     def *(s:Double) =  VectorBreeze3(s*v)
     def x = v(0)
     def y = v(1)
     def z = v(2)

     def unary_-() : VectorBreeze3 = VectorBreeze3(-v)
     def +=(v2: VectorBreeze3)  = v += v2.v

     lazy val toSeq = Seq(x,y,z)
   }

  object VectorBreeze3{
    def apply(x:Double, y: Double, z: Double): VectorBreeze3 = new VectorBreeze3(DenseVector[Double](x,y,z))
    def apply(v: DenseVector[Double]): VectorBreeze3 = new VectorBreeze3(v)

    val ZERO  = VectorBreeze3(0,0,0)
    val ONE  = VectorBreeze3(1,1,1)
    val X = VectorBreeze3(1,0,0)
    val Y = VectorBreeze3(0,1,0)
    val Z = VectorBreeze3(0,0,1)




    implicit val vectorFormat: Format[VectorBreeze3] = new Format[VectorBreeze3]{
      override def writes(vector: VectorBreeze3) =  Json.obj(
        "x" -> vector(0),
        "y" -> vector(1),
        "z" -> vector(2)
      )

      override def reads(json: JsValue) : JsResult[VectorBreeze3] = (
          (JsPath \ "x").read[Double] and
          (JsPath \ "y").read[Double] and
          (JsPath \ "z").read[Double])
        .tupled.map(Function.tupled(VectorBreeze3.apply _) ).reads(json)
      }

  }

}
