package math

import material.{Material, UnshadedMaterial}
import renderer.Hit
import cats.syntax.functor._
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._

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

  implicit val encodeShape: Encoder[Shape] = Encoder.instance {
    case n @ NonEmptyAABB(_, _, _, _, _, _, _) => Json.obj("AABB" -> n.asJson)
    case n @ Sphere(_, _, _)           => Json.obj("Sphere" -> n.asJson)
    case n @ Triangle(_, _, _, _, _, _)   => Json.obj("Triangle" -> n.asJson)
    case n @ Cuboid(_, _, _, _)        => Json.obj("Cuboid" -> n.asJson)
  }

  private val decodeAABB = Decoder[NonEmptyAABB].prepare(_.downField("AABB"))
  private val decodeSphere = Decoder[Sphere].prepare(_.downField("Sphere"))
  private val decodeTriangle = Decoder[Triangle].prepare(_.downField("Triangle"))
  private val decodeCuboid = Decoder[Cuboid].prepare(_.downField("Cuboid"))

  implicit val decodeLightsource: Decoder[Shape] = decodeAABB
    .or(decodeSphere.widen[Shape])
    .or(decodeTriangle.widen[Shape])
    .or(decodeCuboid.widen[Shape])

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
  var materialMap: Map[String, Material] = Map.empty
  def getMaterial(name: String, pos: Vector3, uv_coordinates : Option[(Double,Double)] = None): UnshadedMaterial =
    materialMap.getOrElse(name, Material.DEFAULT_MATERIAL).getMat(pos,uv_coordinates)
}
