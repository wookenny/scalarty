package renderer

import material.UnshadedMaterial
import math.Vector3

final case class Hit(
    distance: Double,
    position: Vector3,
    originalNormal: Vector3,
    material: UnshadedMaterial)
{
  lazy val normal: Vector3 = (originalNormal + material.normalModifier).normalized
}
