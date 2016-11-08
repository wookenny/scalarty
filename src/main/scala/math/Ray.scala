package math
import Math._

final case class Ray(origin: Vector3,
                     direction: Vector3,
                     depth: Int = 0,
                     n: Float = 1f,
                     source: String = "") {
  def march(length: Float) = origin + direction * length

  def reflectedAt(position: Vector3, normal: Vector3): Ray = {
    val dir = (direction - normal * (direction * normal) * 2).normalized
    this
      .copy(origin = position + dir * EPS, direction = dir, depth = depth + 1)
  }

  def refractedAt(position: Vector3, normal: Vector3, newN: Float) = {
    val V = this.direction
    val refractionFactor: Float = this.n / newN
    val negCosI = normal * V
    val (norm, cosI) =
      if (negCosI < 0) (normal, -negCosI) else (-normal, negCosI)

    val sinT2: Float = refractionFactor * refractionFactor * (1f - cosI * cosI)
    if (sinT2 > 1f) //total internal reflection
      None
    else {
      val cosT: Float = Math.sqrt(1f - sinT2).toFloat
      val refractedDir =
        (V * refractionFactor + norm * (refractionFactor * cosI - cosT)).normalized
      Some(
        this.copy(origin = position + refractedDir * EPS,
                  direction = refractedDir,
                  depth = depth + 1,
                  n = newN))
    }
  }
}
