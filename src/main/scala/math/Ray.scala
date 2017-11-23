package math
import Math._
import math.breeze.VectorBreeze3

final case class Ray(origin: VectorBreeze3,
                     direction: VectorBreeze3,
                     depth: Int = 0,
                     n: Double = 1f,
                     source: String = "") {
  def march(length: Double): VectorBreeze3 = origin + direction * length

  def reflectedAt(position: VectorBreeze3, normal: VectorBreeze3): Ray = {
    val dir = (direction - normal * ((direction dot normal) * 2)).normalized
    this.copy(origin = position + dir * EPS, direction = dir, depth = depth + 1)
  }

  def refractedAt(position: VectorBreeze3, normal: VectorBreeze3, newN: Double): Option[Ray] = {
    val V = this.direction
    val refractionFactor: Double = this.n / newN
    val negCosI : Double = normal dot V
    val (norm, cosI) =
      if (negCosI < 0) (normal, -negCosI) else (-normal, negCosI)

    val sinT2 = refractionFactor * refractionFactor * (1f - cosI * cosI)
    if (sinT2 > 1f) //total internal reflection
      None
    else {
      val cosT: Double = scala.math.sqrt(1f - sinT2).toDouble
      val refractedDir : VectorBreeze3 =
        (V * refractionFactor + norm * (refractionFactor * cosI - cosT)).normalized
      Some(
        this.copy(origin = position + (refractedDir * EPS),
                  direction = refractedDir,
                  depth = depth + 1,
                  n = newN))
    }
  }
}
