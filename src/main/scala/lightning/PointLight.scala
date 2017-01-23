package lightning

import color.RGB
import math.Vector3
import play.api.libs.json.{Format, Json}




final case class PointLight(position: Vector3, color: RGB, power: Double){

  def intensity(pos: Vector3) = power /((position-pos)*(position-pos))
}

object PointLight {
  implicit val lightJsonFormat: Format[PointLight] = Json.format[PointLight]
}
