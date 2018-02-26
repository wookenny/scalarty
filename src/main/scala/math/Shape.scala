package math

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import material.{Material, UnshadedColor}
import renderer.Hit

trait Shape {
  def intersect(r: Ray): Option[Hit] //TODO: should only generate needed data, not too much in advance
  def intersect(r: Ray, maxDist: Double): Boolean

  def boundingBox: AABB
  def midpoint: Vector3

  def minX: Double
  def minY: Double
  def minZ: Double
  def maxX: Double
  def maxY: Double
  def maxZ: Double
}

object Shape {
  def unapply(shape: Shape): Option[(String, Json)] = {
    val (prod: Product, sub) = shape match {
      case b: AABB => (b, b.asJson)
      case b: Sphere => (b, b.asJson)
      case b: Triangle => (b, b.asJson)
      case b: Cuboid => (b, b.asJson)
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: String): Shape = {
    (`type` match {
      case "AABB" => decode[AABB](data)
      case "Sphere" => decode[Sphere](data)
      case "Triangle" => decode[Triangle](data)
      case "Cuboid"   => decode[Cuboid](data)
      case _ => Left(Error/*(s"Unknown Shape type: ${`type`}")*/)
    }) match {
      case Right(shape) => shape
      case Left(error) =>
        throw new IllegalArgumentException(s"Could parse the Json as Shape: $data. Error: $error")
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
  var materialMap: Map[String, Material] = Map.empty
  def getMaterial(name: String, pos: Vector3): UnshadedColor =
    materialMap.getOrElse(name, Material.DEFAULT_MATERIAL).getMat(pos)
}
