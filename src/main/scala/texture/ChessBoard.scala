package texture

import color.RGB
import math.ThreeDimensionalFunction

case class ChessBoard(color1: RGB, color2: RGB, steps: Double) extends ThreeDimensionalFunction[RGB]{

  private def mod(x: Double, m: Double) = (x % m + m) % m

  private def inStep(pos: Double): Boolean = mod(pos, 2 * steps) >= steps

  override def eval(x: Double, y: Double, z: Double): RGB = {
    if(inStep(x) ^ inStep(y) ^ inStep(z))
      color1
  else
    color2
  }

}
