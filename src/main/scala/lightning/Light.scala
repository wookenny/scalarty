package lightning

import color.RGB
import math.Vector3
import play.api.libs.json.{Format, Json}

final case class Light(position: Vector3, color: RGB, intensity: Double){

  def power(pos: Vector3) = intensity /((position-pos)*(position-pos))
}

object Light {
  implicit val lightJsonFormat: Format[Light] = Json.format[Light]
}
