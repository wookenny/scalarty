package material

import color.RGB
import math.Vector3
import cats.syntax.functor._
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._

object Material {

  val DEFAULT_MATERIAL =
    SingleColorMaterial("DEFAULT_MATERIAL", RGB(.4, .4, .4), 0.05f, 0.75f, .15f, .05f)

  implicit val encodeMaterial: Encoder[Material] = Encoder.instance {
    case n @ SingleColorMaterial(_, _, _, _, _, _, _, _, _) =>
      Json.obj("SingleColorMaterial" -> n.asJson)
    case n @ CheckerMaterial(_, _, _, _, _, _, _, _, _, _, _) =>
      Json.obj("CheckerMaterial" -> n.asJson)
    case n @ EmissionMaterial(_, _, _) => Json.obj("EmissionMaterial" -> n.asJson)
    case n @ OpenSimplexNoiseMaterial(_, _, _, _, _, _, _, _, _, _, _) =>
      Json.obj("OpenSimplexNoiseMaterial" -> n.asJson)
    case n @ GeneralMaterial(_, _, _, _, _, _, _, _, _, _) =>
      Json.obj("GeneralMaterial" -> n.asJson)
  }

  private val decodeSingleColorMaterial =
    Decoder[SingleColorMaterial].prepare(_.downField("SingleColorMaterial"))
  private val decodeCheckerMaterial =
    Decoder[CheckerMaterial].prepare(_.downField("CheckerMaterial"))
  private val decodeEmissionMaterial =
    Decoder[EmissionMaterial].prepare(_.downField("EmissionMaterial"))
  private val decodeOpenSimplexNoiseMaterial =
    Decoder[OpenSimplexNoiseMaterial].prepare(_.downField("OpenSimplexNoiseMaterial"))
  private val decodeGeneralMaterial =
    Decoder[GeneralMaterial].prepare(_.downField("GeneralMaterial"))

  implicit val decodeLightsource: Decoder[Material] = decodeSingleColorMaterial
    .or(decodeCheckerMaterial.widen[Material])
    .or(decodeEmissionMaterial.widen[Material])
    .or(decodeOpenSimplexNoiseMaterial.widen[Material])
    .or(decodeGeneralMaterial.widen[Material])

}

final case class UnshadedColor(color: RGB,
                               ambient: Double,
                               diffuse: Double,
                               spec: Double,
                               reflective: Double,
                               refractive: Double,
                               n: Double,
                               shininess: Double,
                               emission: Double = 0,
                               normalModifier: Vector3 = Vector3.ZERO)

trait Material {
  def getMat(position: Vector3): UnshadedColor
  def name: String
}
