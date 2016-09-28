package Material

import java.awt.Color
import geometry.geometry.Vector3

object Material {
  val EPS = 0.001
}

case class UnshadedColor(val color: Color, val ambient: Float, val diffuse: Float, val spec: Float, val shininess: Float)

trait Material{
  def getMat(position: Vector3) : UnshadedColor
  //TODO what about UV mapping?
}


case class SingleColorMaterial(val c: Color, val ambient: Float, val diffuse: Float, val spec: Float, val shininess: Float = 64) extends Material {
  require( Math.abs(ambient+diffuse+spec - 1) <= Material.EPS )

  override def getMat(position: Vector3) = UnshadedColor(c, ambient, diffuse, spec, shininess)
}
