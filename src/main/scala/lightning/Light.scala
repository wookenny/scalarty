package lightning

import color.RGB
import math.Vector3
import cats.syntax.functor._
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._

import io.circe.syntax._
import scala.util.Random

trait LightSource {
  def intensity(p: Vector3, positionOnLight: Option[Vector3] = None): Double
  def sample(n: Int): Seq[Vector3]
  def color: RGB
  def position: Vector3
}

final case class PointLight(position: Vector3, color: RGB, power: Double) extends LightSource {
  override def intensity(pos: Vector3, positionOnLight: Option[Vector3]) =
    power / ((position - pos) * (position - pos))
  //no sampling required for point lights
  override def sample(n: Int) = Seq(position)
}

final case class PlaneLight(position: Vector3,
                            width: Double,
                            length: Double,
                            color: RGB,
                            power: Double)
    extends LightSource {

  private def randomOffset(s: Double) = {
    s * (LightSource.rand.nextDouble() - 0.5)
  }

  override def intensity(pos: Vector3, positionOnLight: Option[Vector3]) =
    positionOnLight match {
      case None => power / ((position - pos) * (position - pos))
      case Some(positionOnLightSource) =>
        power / ((positionOnLightSource - pos) * (positionOnLightSource - pos))
    }

  override def sample(n: Int) =
    for {
      x <- (-n + 1).until(n) by 2
      z <- (-n + 1).until(n) by 2
    } yield
      Vector3((x.toDouble / n) * width + position.x + randomOffset(width / n),
              position.y,
              (z.toDouble / n) * length + position.z + randomOffset(length / n))

}

object LightSource {

  val rand: Random = new Random(0)

  implicit val encodeLightsource: Encoder[LightSource] = Encoder.instance {
    case n @ PointLight(_,_,_)     => Json.obj("PointLight"    -> n.asJson)
    case n @ PlaneLight(_,_,_,_,_)   => Json.obj("PlaneLight"  -> n.asJson)
  }

  private val decodePointLight = Decoder[PointLight].prepare(_.downField("PointLight"))
  private val decodePlaneLight = Decoder[PlaneLight].prepare(_.downField("PlaneLight"))


  implicit val decodeLightsource: Decoder[LightSource] = decodePointLight
    .or(decodePlaneLight.widen[LightSource])

}
