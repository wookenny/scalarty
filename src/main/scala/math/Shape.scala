package math

import material.Material
import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._

import play.api.libs.json._
import renderer.Hit

trait Shape {
  def intersect(r: Ray): Option[Hit] //TODO: should only generate needed data, not too much in advance
  def intersect(r: Ray, maxDist: Double): Boolean

  def boundingBox: AABB
  def midpoint: VectorBreeze3

  def minX: Double
  def minY: Double
  def minZ: Double
  def maxX: Double
  def maxY: Double
  def maxZ: Double

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

  def apply(`type`: String, data: JsValue): Shape = {
    (`type` match {
      case "AABB" => Json.fromJson[AABB](data)(aabbFmt)
      case "Sphere" => Json.fromJson[Sphere](data)(sphereFmt)
      case "Triangle" => Json.fromJson[Triangle](data)(triangleFmt)
    }) match {
      case JsSuccess(shape, _) => shape
      case JsError(errors) =>
        throw new IllegalArgumentException(errors.toString)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
  var materialMap: Map[String, Material] = Map.empty
  def getMaterial(name: String, pos: VectorBreeze3) =
    materialMap.getOrElse(name, Material.DEFAULT_MATERIAL).getMat(pos)

  implicit val shapeFmt: Format[Shape] = Json.format[Shape]
  implicit val aabbFmt: Format[AABB] = Json.format[AABB]
  implicit val sphereFmt: Format[Sphere] = Json.format[Sphere]
  implicit val triangleFmt: Format[Triangle] = Json.format[Triangle]
}
