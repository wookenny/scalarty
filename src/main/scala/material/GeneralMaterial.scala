package material

import color.RGB
import material.node._
import math.Vector3

final case class GeneralMaterial(
    name: String,
    colorNode: Option[ColorNode],
    bump: Option[VectorNode],
    ambientNode: Option[ValueNode],
    diffuseNode: Option[ValueNode],
    specNode: Option[ValueNode],
    reflectiveNode: Option[ValueNode],
    refractiveNode: Option[ValueNode],
    n: Option[Double],
    shininessNode: Option[ValueNode],
    absorption: Option[ValueNode]
) extends Material {

  import GeneralMaterial._
  override def getMat(position: Vector3, uv_coordinates: Option[(Double, Double)]): UnshadedMaterial = {
    //TODO: maybe use UV coords
    val ambient = ambientNode.map(_.value(position)).getOrElse(DefaultAmbient)
    val diffuse = diffuseNode.map(_.value(position)).getOrElse(DefaultDiffuse)
    val spec = specNode.map(_.value(position)).getOrElse(DefaultSpec)
    val reflective = reflectiveNode.map(_.value(position)).getOrElse(DefaultReflective)
    val refractive = refractiveNode.map(_.value(position)).getOrElse(DefaultRefractive)
    val sum: Double = ambient + diffuse + reflective + refractive

    UnshadedMaterial(
      colorNode.map(_.value(position)).getOrElse(DefaultColor),
      ambient / sum,
      diffuse / sum,
      spec / sum,
      reflective / sum,
      refractive / sum,
      n.getOrElse(DefaultN),
      shininessNode.map(_.value(position)).getOrElse(0),
      normalModifier = bump.map(_.value(position)).getOrElse(DefaultBump),
      absorption = absorption.map(_.value(position)).getOrElse(0)
    )
  }
}

object GeneralMaterial {
  val DefaultColor = RGB.BLACK
  val DefaultBump = Vector3.ZERO
  val DefaultAmbient = 0d
  val DefaultDiffuse = 1d
  val DefaultSpec = 0d
  val DefaultReflective = 0d
  val DefaultRefractive = 0d
  val DefaultN = 1.33f
  val DefaultShininess = 0d
}
