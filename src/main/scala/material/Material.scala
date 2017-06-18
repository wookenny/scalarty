package material

import color.RGB
import math.Vector3
import play.api.libs.json._

object Material {

  val DEFAULT_MATERIAL =
    SingleColorMaterial("DEFAULT_MATERIAL", RGB(.4, .4, .4), 0.05f, 0.75f, .15f, .05f)

  def unapply(material: Material): Option[(String, JsValue)] = {
    val (prod: Product, sub) = material match {
      case m: SingleColorMaterial =>
        (m, Json.toJson(m)(singleColorMaterialFmt))
      case m: CheckerMaterial =>
        (m, Json.toJson(m)(checkerMaterialFmt))
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: JsValue): Material = {
    (`type` match {
      case "SingleColorMaterial" =>
        Json.fromJson[SingleColorMaterial](data)(singleColorMaterialFmt)
      case "CheckerMaterial" =>
        Json.fromJson[CheckerMaterial](data)(checkerMaterialFmt)
    }) match {
      case JsSuccess(shape, _) => shape
      case JsError(errors) =>
        throw new IllegalArgumentException(errors.toString)
    }
  }

  implicit val materialFmt: Format[Material] = Json.format[Material]
  implicit val singleColorMaterialFmt: Format[SingleColorMaterial] =
    Json.format[SingleColorMaterial]
  implicit val checkerMaterialFmt: Format[CheckerMaterial] =
    Json.format[CheckerMaterial]
}

final case class UnshadedColor(color: RGB,
                               ambient: Double,
                               diffuse: Double,
                               spec: Double,
                               reflective: Double,
                               refractive: Double,
                               n: Double,
                               shininess: Double,
                               emission: Double = 0)

trait Material {
  def getMat(position: Vector3): UnshadedColor
  def name: String

}
