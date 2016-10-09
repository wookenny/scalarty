package geometry


import play.api.libs.json.Json



case class Light(position: Vector3, color: RGB, intensity: Float)

object Light{
  implicit val lightJsonFormat = Json.format[Light]
}

