package lightning

import color.RGB
import math.breeze.VectorBreeze3
import play.api.libs.json._
import math.breeze.VectorBreeze3._
import scala.util.Random

trait LightSource {
  def intensity(p: VectorBreeze3, positionOnLight: Option[VectorBreeze3] = None): Double
  def sample(n: Int): Seq[VectorBreeze3]
  def color: RGB
  def position: VectorBreeze3
}

final case class PointLight(position: VectorBreeze3, color: RGB, power: Double) extends LightSource {
  override def intensity(pos: VectorBreeze3, positionOnLight: Option[VectorBreeze3]) =
    power / ((position - pos) dot (position - pos))
  //no sampling required for point lights
  override def sample(n: Int) = Seq(position)
}

final case class PlaneLight(position: VectorBreeze3,
                            width: Double,
                            length: Double,
                            color: RGB,
                            power: Double)
    extends LightSource {

  private def randomOffset(s: Double) = {
    s * (LightSource.rand.nextDouble() - 0.5)
  }

  override def intensity(pos: VectorBreeze3, positionOnLight: Option[VectorBreeze3]) =
    positionOnLight match {
      case None => power / ((position - pos) dot (position - pos))
      case Some(positionOnLightSource) =>
        power / ((positionOnLightSource - pos) dot (positionOnLightSource - pos))
    }

  override def sample(n: Int) =
    for {
      x <- (-n + 1).until(n) by 2
      z <- (-n + 1).until(n) by 2
    } yield
      VectorBreeze3((x.toDouble / n) * width + position(0) + randomOffset(width / n),
              position(1),
              (z.toDouble / n) * length + position(2) + randomOffset(length / n))

    //TODO def ==, != ??
}

object LightSource {

  val rand: Random = new Random(0)

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
