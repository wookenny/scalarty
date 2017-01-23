package lightning

import color.RGB
import math.Vector3
import play.api.libs.json._

trait LightSource{
  def intensity(p: Vector3) : Double
  def sample(n: Int) : Iterator[Vector3]

  //TODO: remove these? At least remove position
  //TODO: color might depende on position
  val color: RGB
  val position: Vector3
}

final case class PointLight(position: Vector3, color: RGB, power: Double) extends LightSource{
  override def intensity(pos: Vector3) = power /((position-pos)*(position-pos))
  //no sampling required for point lights
  override def sample(n: Int) = Iterator(position)
}

object LightSource {

  def unapply(light: LightSource): Option[(String, JsValue)] = {
    val (prod: Product, sub) = light match {
      case l: PointLight => (l, Json.toJson(l)(pointLightFmt))
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: JsValue): LightSource = {
    (`type` match {
      case "PointLight" => Json.fromJson[PointLight](data)(pointLightFmt)
    }) match {
      case JsSuccess(light, _) => light
      case JsError(errors) =>
        throw new IllegalArgumentException(errors.toString)
    }
  }

  implicit val lightSourceFmt: Format[LightSource] = Json.format[LightSource]
  implicit val pointLightFmt: Format[PointLight] = Json.format[PointLight]
}