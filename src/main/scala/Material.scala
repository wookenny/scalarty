package Material

import geometry.{RGB, Vector3}
import play.api.libs.json.{JsValue, Json}

object Material {
  val EPS = 0.001

  final val DEFAULT_MATERIAL = SingleColorMaterial(RGB(.4,.4,.4),0.05f,0.75f,.15f,.05f)

  def unapply(material: Material): Option[(String, JsValue)] = {
    val (prod: Product, sub) = material match {
      case m: SingleColorMaterial => (m, Json.toJson(m)(singleColorMaterialFmt))
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: JsValue): Material = {
    (`type` match {
      case "SingleColorMaterial" => Json.fromJson[SingleColorMaterial](data)(singleColorMaterialFmt)
    }).get
  }

  implicit val materialFmt = Json.format[Material]
  implicit val singleColorMaterialFmt = Json.format[SingleColorMaterial]
}

case class UnshadedColor(color: RGB, ambient: Float, diffuse: Float, spec: Float, reflective: Float, shininess: Float)

trait Material{
  def getMat(position: Vector3) : UnshadedColor
}

case class SingleColorMaterial(c: RGB, ambient: Float, diffuse: Float, spec: Float, reflective: Float = 0.05f, shininess: Float = 64) extends Material {
  require( Math.abs(ambient+diffuse+spec+reflective - 1) <= Material.EPS )

  override def getMat(position: Vector3) = UnshadedColor(c, ambient, diffuse, spec, reflective, shininess)
}
