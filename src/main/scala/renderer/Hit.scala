package renderer

import material.UnshadedColor
import math.Vector3

final case class Hit(distance: Double,
                     position: Vector3,
                     originalNormal: Vector3,
                     color: UnshadedColor) {
  lazy val normal = (originalNormal + color.normalModifier).normalized
}
