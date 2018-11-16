package material

import color.RGB
import math.Vector3
import math.Math._

final case class CheckerMaterial(
    name: String,
    c1: RGB,
    c2: RGB,
    steps: Double,
    ambient: Double,
    diffuse: Double,
    spec: Double,
    reflective: Double = 0,
    refractive: Double = 0,
    n: Double = 1.33f,
    shininess: Double = 64,
    absorption: Option[Double] = None) extends Material {
  require(Math.abs(ambient + diffuse + spec + reflective + refractive - 1) <= EPS)

  private def mod(x: Double, m: Double) = (x % m + m) % m

  private def inStep(pos: Double): Boolean = mod(pos, 2 * steps) >= steps
  private def inStep(pos: Vector3): Boolean =
    inStep(pos.x) ^ inStep(pos.y) ^ inStep(pos.z)
  override def getMat(position: Vector3, uv_coordinates: Option[(Double, Double)]) =
    UnshadedMaterial(
      if (inStep(position)) c1 else c2,
      ambient,
      diffuse,
      spec,
      reflective,
      refractive,
      n,
      shininess,
      absorption.getOrElse(0)
    )
}
