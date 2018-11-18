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
    case n @ SingleColorMaterial(_, _, _, _, _, _, _, _, _, _) =>
      Json.obj("SingleColorMaterial" -> n.asJson)
    case n @ CheckerMaterial(_, _, _, _, _, _, _, _, _, _, _, _) =>
      Json.obj("CheckerMaterial" -> n.asJson)
    case n @ EmissionMaterial(_, _, _) => Json.obj("EmissionMaterial" -> n.asJson)
    case n @ OpenSimplexNoiseMaterial(_, _, _, _, _, _, _, _, _, _, _) =>
      Json.obj("OpenSimplexNoiseMaterial" -> n.asJson)
    case n @ GeneralMaterial(_, _, _, _, _, _, _, _, _, _, _) =>
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

case class UnshadedMaterial(
    ambientColor: RGB,
    diffuseColor: RGB,
    specularColor: RGB,
    reflectiveColor: RGB,
    absorptionColor: RGB,
    shininess: Double,
    n: Double,
    transparency: Double,
    normalModifier: Vector3,
    emission: RGB
) {}

object UnshadedMaterial {

  val emptyMaterial = UnshadedMaterial(
    RGB.BLACK,
    RGB.BLACK,
    RGB.BLACK,
    RGB.BLACK,
    RGB.BLACK,
    0,
    0,
    0,
    Vector3.ZERO,
    RGB.BLACK
  )

  def from(
      color: RGB,
      ambient: Double,
      diffuse: Double,
      spec: Double,
      reflective: Double,
      refractive: Double,
      n: Double,
      shininess: Double,
      emissionCoeffient: Double = 0,
      normalModifier: Vector3 = Vector3.ZERO,
      absorption: Double = 0
  ): UnshadedMaterial =
    UnshadedMaterial(
      ambientColor = color * ambient,
      diffuseColor = color * diffuse,
      specularColor = RGB(spec, spec, spec),
      reflectiveColor = RGB.WHITE * reflective, //reflect without color change?
      absorptionColor = (RGB.WHITE - color) * absorption,
      shininess = shininess,
      n = n,
      transparency = refractive,
      normalModifier = normalModifier,
      emission = color * emissionCoeffient
    )

  def fromWaveFrontMaterial(Ka: RGB, Kd: RGB, Ks: RGB, Tr: Double, Ns: Double) =
    UnshadedMaterial(
      ambientColor = Ka,
      diffuseColor = Kd,
      specularColor = Ks,
      reflectiveColor = Ks,
      absorptionColor = RGB.BLACK,
      shininess = Ns,
      n = 0,
      transparency = Tr,
      normalModifier = Vector3.ZERO,
      emission = RGB.BLACK
    )
}

//TODO: Implement and use the new trait
// * add parsing in ObjMaterialReaading
// * add example with texture map and material
//run -i scenes/cornell.json -o test.before.png --> 96s/115s/116s/131s
// 111/97/

trait Material {
  def getMat(position: Vector3): UnshadedMaterial = getMat(position, None)
  def getMat(position: Vector3, uv_coordinates: Option[(Double, Double)]): UnshadedMaterial
  def name: String
}
