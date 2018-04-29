package texture

import math.ThreeDimensionalFunction
import Math.abs

case class ChessBoard[A](even: A, odd: A, stepSize: Double) extends ThreeDimensionalFunction[A] {
  require(stepSize > 0)
  private def mod(x: Double, m: Double) = (x % m + abs(m)) % m

  private def inStep(pos: Double): Boolean = mod(pos, 2 * stepSize) >= stepSize

  override def eval(x: Double, y: Double, z: Double): A =
    if (inStep(x) ^ inStep(y) ^ inStep(z))
      even
    else odd

}
