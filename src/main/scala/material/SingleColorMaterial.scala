package material

import color.RGB
import math.Math._
import math.Vector3

final case class SingleColorMaterial(
    name: String,
    c: RGB,
    ambient: Double,
    diffuse: Double,
    spec: Double,
    reflective: Double = 0,
    refractive: Double = 0,
    n: Double = 1.33f,
    shininess: Double = 64,
    absorption : Double = 0
) extends Material {
  require(Math.abs(ambient + diffuse + spec + reflective + refractive - 1) <= EPS)

  override def getMat(position: Vector3) = UnshadedMaterial(c, ambient, diffuse, spec, reflective, refractive, n, shininess, absorption = absorption)

}

final case class EmissionMaterial(name: String, color: RGB, intensity: Double) extends Material {

  override def getMat(position: Vector3) =
    UnshadedMaterial(color, 0, 0, 0, 0, 0, 0, 0, intensity)
  //TODO: handle emission correctly in tracer,
  //TODO: for shadows and lightning, we require some sampling here
}
