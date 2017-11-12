package math

import play.api.libs.json._
import play.api.libs.functional.syntax._
import _root_.breeze.linalg._
import play.api.libs.json.Reads._

object breeze{
   type VectorBreeze3 = DenseVector[Double]

  object VectorBreeze3{
    val ZERO : VectorBreeze3 = DenseVector.zeros(3)
    val ONE : VectorBreeze3 = DenseVector.ones(3)
    val X : VectorBreeze3 = DenseVector[Double](1,0,0)
    val Y : VectorBreeze3 = DenseVector[Double](0,1,0)
    val Z : VectorBreeze3 = DenseVector[Double](0,0,1)

    def length(v: VectorBreeze3): Double = scala.math.sqrt(v dot v)
    def normalized(v: VectorBreeze3)  = v/length(v)
    def cross(v1: VectorBreeze3, v2: VectorBreeze3) : VectorBreeze3 = DenseVector[Double](v1(1) * v2(2) - v1(2) * v2(1), v1(2) * v2(0) - v1(0) * v2(2), v1(0) * v2(1) - v1(1) * v2(0))


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
        .tupled.map(Function.tupled(VectorBreeze3.from)).reads(json)
      }

    def from(x:Double,y:Double,z:Double ) = DenseVector[Double](x,y,z)

    def ~=(a: VectorBreeze3, b: VectorBreeze3, delta: Double = 0.001): Boolean = length(a-b) < delta
  }

}


final case class Vector3(x: Double, y: Double, z: Double) {

  def /(s: Double) = Vector3(x / s, y / s, z / s)
  def *(s: Double) = Vector3(s * x, s * y, s * z)

  def +(p: Vector3) = Vector3(x + p.x, y + p.y, z + p.z)
  def -(p: Vector3) = Vector3(x - p.x, y - p.y, z - p.z)
  def *(p: Vector3): Double = x * p.x + y * p.y + z * p.z

  def unary_-(): Vector3 = this * (-1)
  def unary_+(): Vector3 = this
  def ~=(p: Vector3, delta: Double = 0.001): Boolean = (this - p).length < delta

  def length: Double = scala.math.sqrt(this * this)
  def dist(p: Vector3): Double = (this - p).length
  def normalized: Vector3 = this / length
  def cross(p: Vector3) =
    Vector3(y * p.z - z * p.y, z * p.x - x * p.z, x * p.y - y * p.x)

  def pow(exp: Double) =
    Vector3(scala.math.pow(x, exp), scala.math.pow(y, exp), scala.math.pow(z, exp))
  def expf =
    Vector3(scala.math.exp(x), scala.math.exp(y), scala.math.exp(z))

}

object Vector3 {

  val ZERO = Vector3(0, 0, 0)
  val ONE = Vector3(1, 1, 1)
  val X = Vector3(1, 0, 0)
  val Y = Vector3(0, 1, 0)
  val Z = Vector3(0, 0, 1)

  implicit val vectorJsonFormat: Format[Vector3] = Json.format[Vector3]
}
