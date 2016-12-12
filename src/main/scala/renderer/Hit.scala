package renderer

import material.UnshadedColor
import math.Vector3

final case class Hit(val distance: Double,
                     val position: Vector3,
                     val normal: Vector3,
                     val color: UnshadedColor)
