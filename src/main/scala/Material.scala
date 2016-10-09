package Material

import geometry.{RGB, Vector3}

object Material {
  val EPS = 0.001
}

case class UnshadedColor(color: RGB, ambient: Float, diffuse: Float, spec: Float, reflective: Float, shininess: Float)

trait Material{

  def getMat(position: Vector3) : UnshadedColor
  //TODO what about UV mapping?
}


case class SingleColorMaterial(c: RGB, ambient: Float, diffuse: Float, spec: Float, reflective: Float = 0.05f, shininess: Float = 64) extends Material {
  require( Math.abs(ambient+diffuse+spec+reflective - 1) <= Material.EPS )

  override def getMat(position: Vector3) = UnshadedColor(c, ambient, diffuse, spec, reflective, shininess)
}
