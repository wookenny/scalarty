package texture

import generators.Generators._
import org.scalacheck.Gen
import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.Prop.forAll

object TestType extends Enumeration {
  type TestType = Value
  val Even, Odd = Value
}


class ChessBoardSpec extends Specification with ScalaCheck {
  def is = s2"""
   A Chessboard texture should
    have the other element for an offset of once the stepsize in one dimension $testAlternatingPattern
  """

  val stepSizeGen = Gen.choose(0.01, 20d)

  def testAlternatingPattern = forAll(stepSizeGen, threeDimensionalPoint){ (stepSize:Double, point:(Double,Double,Double)) =>

    import TestType._
    val chessboard = ChessBoard(Even, Odd, stepSize)

    val (x,y,z) = point
    val colorReferece = chessboard.eval(x,y,z)

    ((x+stepSize,y,z) !== colorReferece)    and
       ((x,y+stepSize,z) !== colorReferece) and
       ((x,y,z+stepSize) !== colorReferece)
  }

}
