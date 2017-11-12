package renderer

import material.UnshadedColor
import math.breeze.VectorBreeze3

final case class Hit(distance: Double,
                     position: VectorBreeze3,
                     normal: VectorBreeze3,
                     color: UnshadedColor)
