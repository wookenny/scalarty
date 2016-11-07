package math

import cats.data.Xor
import material.Material
import play.api.libs.json._
import renderer.Hit

trait Shape {
  def intersect(r: Ray): Option[Hit] //TODO: should only generate needed data, not too much in advance
  def intersect(r: Ray, maxDist: Float): Boolean
}

object Shape {
  def unapply(shape: Shape): Option[(String, JsValue)] = {
    val (prod: Product, sub) = shape match {
      case b: AABB => (b, Json.toJson(b)(aabbFmt))
      case b: Sphere => (b, Json.toJson(b)(sphereFmt))
      case b: Triangle => (b, Json.toJson(b)(triangleFmt))
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: JsValue): Shape = {// Xor[Error,Shape] = {
    (`type` match {
      case "AABB" => Json.fromJson[AABB](data)(aabbFmt)
      case "Sphere" => Json.fromJson[Sphere](data)(sphereFmt)
      case "Triangle" => Json.fromJson[Triangle](data)(triangleFmt)
    }).get /* match {
      case JsSuccess(v, _) => Xor.right(v)
      case JsError(e) =>   Xor.left(new Error(e.toString()))
    }*/
  }

  @SuppressWarnings(
    Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
  var materialMap: Map[String, Material] = Map.empty
  def getMaterial(name: String, pos: Vector3) =
    materialMap.getOrElse(name, Material.DEFAULT_MATERIAL).getMat(pos)

  implicit val shapeFmt: Format[Shape] = Json.format[Shape]
  implicit val aabbFmt: Format[AABB] = Json.format[AABB]
  implicit val sphereFmt: Format[Sphere] = Json.format[Sphere]
  implicit val triangleFmt: Format[Triangle] = Json.format[Triangle]
}
