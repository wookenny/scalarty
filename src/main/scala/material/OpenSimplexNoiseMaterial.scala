package material

import color.RGB
import math.Vector3
import math.Math._

import noise.OpenSimplex
import noise.MultilayerNoise._

final case class OpenSimplexNoiseMaterial(name: String,
                                          color1: RGB, color2: RGB,
                                          scale : Float,
                                          ambient: Double,
                                          diffuse: Double,
                                          spec: Double,
                                          reflective: Double = 0,
                                          refractive: Double = 0,
                                          n: Double = 1.33f,
                                          shininess: Double = 64) extends Material {
  require(Math.abs(ambient + diffuse + spec + reflective + refractive - 1) <= EPS)

  implicit val noise =  new OpenSimplex(0)

  private def colorAt(v: Vector3) = {
    //val n = noise.value(v.x*scale, v.y*scale, v.z*scale)
    val n = multilayerNoise3(v.x, v.y, v.z, 1/scale)
    val range = 0.5*Math.sqrt(3)
    val share = (range + n) / (2*range)
    color1 * share + color2 * (1-share)
  }

  override def getMat(position: Vector3) =
    UnshadedColor(colorAt(position), ambient, diffuse, spec, reflective, refractive, n, shininess)
}
