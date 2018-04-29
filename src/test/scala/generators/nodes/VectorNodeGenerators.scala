package generators.nodes

import generators.Generators
import material.node._
import org.scalacheck.Gen
import generators.Generators.{positiveDouble, vector3}
import org.scalacheck.Arbitrary.arbitrary
import ValueNodeGenerators._


object VectorNodeGenerators {

  val MaxRecursionLevel = 3

  // Vector nodes
    def constantVector: Gen[ConstantVector] =
      for {
        value <- Generators.vector3
      } yield ConstantVector(value)


  /* Recursive node types */

  def colorToVector: Gen[ColorToVector] = colorToVector(MaxRecursionLevel)
  private def colorToVector(maxDepth: Int): Gen[ColorToVector] = for {
    colorNode <- ColorNodeGenerators.randomColorNode(maxDepth)
  } yield ColorToVector(colorNode)


  def singleValueToVector: Gen[SingleValueToVector] = singleValueToVector(MaxRecursionLevel)
  private def singleValueToVector(maxDepth: Int): Gen[SingleValueToVector] =
    for {
      node <- ValueNodeGenerators.randomValueNode(maxDepth)
    } yield SingleValueToVector(node)

  def valuesToVector: Gen[ValuesToVector] = valuesToVector(MaxRecursionLevel)
  private def valuesToVector(maxDepth: Int): Gen[ValuesToVector] =
    for {
      node1 <- randomValueNode(maxDepth)
      node2 <- randomValueNode(maxDepth)
      node3 <- randomValueNode(maxDepth)
    } yield ValuesToVector(node1, node2, node3)

  def clampVector: Gen[ClampVector] = clampVector(MaxRecursionLevel)
  private def clampVector(maxDepth: Int): Gen[ClampVector] = for {
    node <- randomVectorNode(maxDepth)
    minValue <- arbitrary[Double]
    maxValue <- Gen.choose(minValue,Double.MaxValue)
  } yield ClampVector(node, minValue, maxValue)

  def modVector: Gen[ModVector] = modVector(MaxRecursionLevel)
  private def modVector(maxDepth: Int): Gen[ModVector] = for {
    node <- randomVectorNode(maxDepth)
    mod <- arbitrary[Double]
  } yield ModVector(node,mod)

  def addVector: Gen[AddVector] = addVector(MaxRecursionLevel)
  private def addVector(maxDepth: Int): Gen[AddVector] = for {
    node1 <- randomVectorNode(maxDepth)
    node2 <- randomVectorNode(maxDepth)
  } yield AddVector(node1, node2)

  def subtractVector: Gen[SubtractVector] = subtractVector(MaxRecursionLevel)
  private def subtractVector(maxDepth: Int): Gen[SubtractVector] = for {
    node1 <- randomVectorNode(maxDepth)
    node2 <- randomVectorNode(maxDepth)
  } yield SubtractVector(node1, node2)

  def multiplyVector: Gen[MultiplyVector] = multiplyVector(MaxRecursionLevel)
  private def multiplyVector(maxDepth: Int): Gen[MultiplyVector] = for {
    node1 <- randomVectorNode(maxDepth)
    node2 <- randomVectorNode(maxDepth)
  } yield MultiplyVector(node1, node2)

  def mixVector: Gen[MixVector] = mixVector(MaxRecursionLevel)
  private def mixVector(maxDepth: Int): Gen[MixVector] = for {
    node1 <- randomVectorNode(maxDepth)
    node2 <- randomVectorNode(maxDepth)
    mixValue <- randomValueNode(maxDepth)
  } yield MixVector(node1, node2, mixValue)

  def randomVectorNode: Gen[VectorNode] = randomVectorNode(MaxRecursionLevel)
  def randomVectorNode(maxLevel: Int): Gen[VectorNode] =
    if (maxLevel <= 0)
      constantVector
    else {
      val level = maxLevel - 1
      Gen.oneOf(
        constantVector,
        colorToVector(level),
        valuesToVector(level),
        singleValueToVector(level),
        clampVector(level),
        modVector(level),
        addVector(level),
        subtractVector(level),
        multiplyVector(level),
        mixVector(level)
      )
    }

}
