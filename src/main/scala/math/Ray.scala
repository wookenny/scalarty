package math
import Math._

final case class Ray(
    origin: Vector3,
    direction: Vector3,
    depth: Int = 0,
    n: Double = 1f,
    source: String = "",
    insideMedia: Boolean = false
) {

  def marchedRay(length: Double) = copy(origin = this.origin + direction * length)
  def march(length: Double) = origin + direction * length

  def reflectedAt(position: Vector3, normal: Vector3): Ray = {
    val dir = (direction - normal * (direction * normal) * 2).normalized
    this
      .copy(origin = position + dir * EPS, direction = dir, depth = depth + 1, insideMedia = false)
  }

  def refractedAt(position: Vector3, normal: Vector3, newN: Double) = {
    val V = this.direction
    val refractionFactor: Double = this.n / newN
    val negCosI = normal * V
    val (norm, cosI) =
      if (negCosI < 0) (normal, -negCosI) else (-normal, negCosI)

    val sinT2 = refractionFactor * refractionFactor * (1f - cosI * cosI)
    if (sinT2 > 1f) //total internal reflection
      None
    else {
      val cosT: Double = scala.math.sqrt(1f - sinT2).toDouble
      val refractedDir =
        (V * refractionFactor + norm * (refractionFactor * cosI - cosT)).normalized
      Some(
        Ray(
          origin = position + refractedDir * EPS,
          direction = refractedDir,
          depth = this.depth + 1,
          n = newN,
          source = this.source,
          insideMedia = !this.insideMedia //TODO: use normalto set this instead of alternating
        )
      )
    }
  }
}
