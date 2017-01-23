package lightning

import color.RGB
import math.Vector3
import play.api.libs.json._

import scala.util.Random

trait LightSource{
  def intensity(p: Vector3, positionOnLight : Option[Vector3] = None) : Double
  def sample(n: Int) : Seq[Vector3]
  def color: RGB
  def position: Vector3
}

final case class PointLight(position: Vector3, color: RGB, power: Double) extends LightSource{
  override def intensity(pos: Vector3, positionOnLight: Option[Vector3]) = power /((position-pos)*(position-pos))
  //no sampling required for point lights
  override def sample(n: Int) = Seq(position)
}

final case class PlaneLight(position: Vector3, width: Double, length: Double,
                            color: RGB, power: Double) extends LightSource{

  private def randomOffset(n: Int, s: Double) = s/n * (LightSource.rand.nextDouble() - 0.5)

  override def intensity(pos: Vector3, positionOnLight: Option[Vector3]) =
    positionOnLight match {
      case None => power /((position-pos)*(position-pos))
      case Some(positionOnLightSource) =>  power /((positionOnLightSource-pos)*(positionOnLightSource-pos))
    }

  override def sample(n: Int) =
    for {
      x <- (-n+1) to (n-1) by 2
      y <- (-n+1) to (n-1) by 2
    } yield Vector3( (x.toDouble/n)*width + position.x  + randomOffset(n,width),
                     (y.toDouble/n)*length + position.y + randomOffset(n,length),
                      position.z)


}

object LightSource {

  val rand : Random = new Random(0)

  def unapply(light: LightSource): Option[(String, JsValue)] = {
    val (prod: Product, sub) = light match {
      case l: PointLight => (l, Json.toJson(l)(pointLightFmt))
      case l: PlaneLight => (l, Json.toJson(l)(planeLightFmt))
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: JsValue): LightSource = {
    (`type` match {
      case "PointLight" => Json.fromJson[PointLight](data)(pointLightFmt)
      case "PlaneLight" => Json.fromJson[PlaneLight](data)(planeLightFmt)
    }) match {
      case JsSuccess(light, _) => light
      case JsError(errors) =>
        throw new IllegalArgumentException(errors.toString)
    }
  }

  implicit val lightSourceFmt: Format[LightSource] = Json.format[LightSource]
  implicit val pointLightFmt: Format[PointLight] = Json.format[PointLight]
  implicit val planeLightFmt: Format[PlaneLight] = Json.format[PlaneLight]

}