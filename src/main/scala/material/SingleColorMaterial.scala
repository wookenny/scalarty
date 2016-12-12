package material

import color.RGB
import math.Vector3
import math.Math._

final case class SingleColorMaterial(name: String,
                                     c: RGB,
                                     ambient: Double,
                                     diffuse: Double,
                                     spec: Double,
                                     reflective: Double = 0,
                                     refractive: Double = 0,
                                     n: Double = 1.33f,
                                     shininess: Double = 64)
    extends Material {
  require(
    Math.abs(ambient + diffuse + spec + reflective + refractive - 1) <= EPS)

  override def getMat(position: Vector3) =
    UnshadedColor(c,
                  ambient,
                  diffuse,
                  spec,
                  reflective,
                  refractive,
                  n,
                  shininess)
}
