package geometry


import play.api.libs.json.{Format, Json}



final case class Light(position: Vector3, color: RGB, intensity: Float)

object Light{
  implicit val lightJsonFormat : Format[Light] = Json.format[Light]
}

