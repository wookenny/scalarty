package material

import color.RGB
import math.Vector3
import math.Math.EPS
import noise.{MultilayerNoise, OpenSimplex}
import math.Gradient._
import math.Gradient.implicits._

final case class OpenSimplexNoiseMaterial(
    name: String,
    color1: RGB,
    color2: RGB,
    size: Float,
    ambient: Double,
    diffuse: Double,
    spec: Double,
    reflective: Double = 0,
    refractive: Double = 0,
    n: Double = 1.33f,
    shininess: Double = 64
) extends Material {
  require(Math.abs(ambient + diffuse + spec + reflective + refractive - 1) <= EPS)

  //implicit val noise =  new OpenSimplex(0, size)
  implicit val noise: MultilayerNoise = new MultilayerNoise(0, size)

  private def colorAt(v: Vector3) = {
    val n = noise.eval(v.x, v.y, v.z)
    val range = 0.5 * Math.sqrt(3)
    val share = (range + n) / (2 * range)
    color1 * share + color2 * (1 - share)
  }
  private def gradientAt(v: Vector3) = (Vector3.apply _).tupled(gradient[Double](v.x, v.y, v.z))

  override def getMat(position: Vector3) =
    UnshadedMaterial(
      colorAt(position),
      ambient,
      diffuse,
      spec,
      reflective,
      refractive,
      n,
      shininess,
      normalModifier = gradientAt(position) * 0.005
    )
}
