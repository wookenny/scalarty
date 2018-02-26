package lightning

import color.RGB
import math.Vector3
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

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

  def unapply(light: LightSource): Option[(String, Json)] = {
    val (prod: Product, sub) = light match {
      case l: PointLight => (l, l.asJson)
      case l: PlaneLight => (l, l.asJson)
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: String): LightSource = {
    (`type` match {
      case "PointLight" => decode[PointLight](data)
      case "PlaneLight" => decode[PlaneLight](data)
      case _ => Left(Error/*(s"Unknown Light type: ${`type`}")*/)
    }) match {
      case Right(light) => light
      case Left(error) =>
        throw new IllegalArgumentException(s"Could parse the Json as Light: $data. Error: $error")
    }
  }
}
