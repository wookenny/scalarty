package geometry


import play.api.libs.json.{JsPath, Json, Reads}
import play.api.libs.json._
import play.api.libs.functional.syntax._


case class Light(position: Vector3, color: RGB, intensity: Float)

object Light{
  implicit val lightJsonFormat = Json.format[Light]
}

