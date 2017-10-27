package renderer

import material.UnshadedColor
import math.Vector3

final case class Hit(distance: Double,
                     position: Vector3,
                     normal: Vector3,
                     color: UnshadedColor)
