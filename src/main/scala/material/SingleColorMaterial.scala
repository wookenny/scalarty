package material

import color.RGB
import math.Vector3
import math.Math._

final case class SingleColorMaterial(name: String,
                                     c: RGB,
                                     ambient: Float,
                                     diffuse: Float,
                                     spec: Float,
                                     reflective: Float = 0.05f,
                                     refractive: Float = 0,
                                     n: Float = 1.33f,
                                     shininess: Float = 64)
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
