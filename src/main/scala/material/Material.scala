package material

import color.RGB
import math.Vector3
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
object Material {

  val DEFAULT_MATERIAL =
    SingleColorMaterial("DEFAULT_MATERIAL", RGB(.4, .4, .4), 0.05f, 0.75f, .15f, .05f)

  def unapply(material: Material): Option[(String, Json)] = {
    val (prod: Product, sub) = material match {
      case m: SingleColorMaterial => (m, m.asJson)
      case m: CheckerMaterial => (m, m.asJson)
      case m: EmissionMaterial => (m, m.asJson)
      case m: OpenSimplexNoiseMaterial => (m, m.asJson)
      case m: GeneralMaterial =>  (m, m.asJson)
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: String): Material = {
    (`type` match {
      case "SingleColorMaterial" =>
        decode[SingleColorMaterial](data)
      case "CheckerMaterial" =>
        decode[CheckerMaterial](data)
      case "EmissionMaterial" =>
        decode[EmissionMaterial](data)
      case "SimplexMaterial" =>
        decode[OpenSimplexNoiseMaterial](data)
      case "GeneralMaterial" => decode[GeneralMaterial](data)
      case materialType =>
        Left(Error/*(s"Unknown Material type: $materialType")*/)
    }) match {
      case Right(shape) => shape
      case Left(error) =>
        throw new IllegalArgumentException(s"Could parse the Json as material: $error")
    }
  }
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