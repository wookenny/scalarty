package material

import color.RGB
import math.Vector3
import math.Math._
import math.breeze.VectorBreeze3

final case class CheckerMaterial(name: String,
                                 c1: RGB,
                                 c2: RGB,
                                 steps: Double,
                                 ambient: Double,
                                 diffuse: Double,
                                 spec: Double,
                                 reflective: Double = 0,
                                 refractive: Double = 0,
                                 n: Double = 1.33f,
                                 shininess: Double = 64)
    extends Material {
  require(Math.abs(ambient + diffuse + spec + reflective + refractive - 1) <= EPS)

  private def mod(x: Double, m: Double) = (x % m + m) % m

  private def inStep(pos: Double): Boolean = mod(pos, 2 * steps) >= steps
  private def inStep(pos: VectorBreeze3): Boolean = pos.map( inStep ).reduce(_^_)
  override def getMat(position: VectorBreeze3) =
    UnshadedColor(if (inStep(position)) c1 else c2,
                  ambient,
                  diffuse,
                  spec,
                  reflective,
                  refractive,
                  n,
                  shininess)
}
