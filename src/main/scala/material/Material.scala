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
      case m: EmissionMaterial =>
        (m, Json.toJson(m)(emissionMaterialFmt))
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: JsValue): Material = {
    (`type` match {
      case "SingleColorMaterial" =>
        Json.fromJson[SingleColorMaterial](data)(singleColorMaterialFmt)
      case "CheckerMaterial" =>
        Json.fromJson[CheckerMaterial](data)(checkerMaterialFmt)
      case "EmissionMaterial" =>
        Json.fromJson[EmissionMaterial](data)(emissionMaterialFmt)
      case materialType =>
        throw new IllegalArgumentException(s"Unknown Material type: $materialType")
    }) match {
      case JsSuccess(shape, _) => shape
      case JsError(_) =>
        throw new IllegalArgumentException(s"Could parse the Json as material: $data")
    }
  }

  implicit val materialFmt = Json.format[Material]
  implicit val singleColorMaterialFmt = Json.format[SingleColorMaterial]
  implicit val checkerMaterialFmt = Json.format[CheckerMaterial]
  implicit val emissionMaterialFmt = Json.format[EmissionMaterial]
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
