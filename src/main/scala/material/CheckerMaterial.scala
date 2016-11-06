package material

import color.RGB
import math.Vector3
import math.Math._

final case class CheckerMaterial(name: String,
                                 c1: RGB,
                                 c2: RGB,
                                 steps: Float,
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

  private def mod(x: Float, m: Float) = (x % m + m) % m

  private def inStep(pos: Float): Boolean = mod(pos, 2 * steps) >= steps
  private def inStep(pos: Vector3): Boolean =
    inStep(pos.x) ^ inStep(pos.y) ^ inStep(pos.z)
  override def getMat(position: Vector3) =
    UnshadedColor(if (inStep(position)) c1 else c2,
                  ambient,
                  diffuse,
                  spec,
                  reflective,
                  refractive,
                  n,
                  shininess)
}
