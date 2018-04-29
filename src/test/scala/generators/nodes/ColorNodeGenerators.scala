package generators.nodes

import generators.Generators
import material.node._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object ColorNodeGenerators {

  val MaxRecursionLevel = 3

  def constantColor: Gen[ConstantColor] =
    for {
      color <- Generators.rgb
    } yield ConstantColor(color)

  def checkerColor: Gen[CheckerColor] = for {
    color1 <- Generators.rgb
    color2 <- Generators.rgb
    stepSize <- arbitrary[Double]
  } yield CheckerColor(color1, color2, stepSize)

  def pointFallColor: Gen[PointFallColor] = for {
    color1 <- Generators.rgb
    color2 <- Generators.rgb
    center <- Generators.vector3
  } yield PointFallColor(color1, color2, center)


  /* Recursive nodes */
  def vectorToColor: Gen[VectorToColor] = vectorToColor(MaxRecursionLevel)
  private def vectorToColor(maxDepth: Int): Gen[VectorToColor] = for {
    node <- VectorNodeGenerators.randomVectorNode(maxDepth)
  } yield VectorToColor(node)

  def valuesToColor: Gen[ValuesToColor] = valuesToColor(MaxRecursionLevel)
  private def valuesToColor(maxDepth: Int): Gen[ValuesToColor] = for {
    node1 <- ValueNodeGenerators.randomValueNode(maxDepth)
    node2 <- ValueNodeGenerators.randomValueNode(maxDepth)
    node3 <- ValueNodeGenerators.randomValueNode(maxDepth)
  } yield ValuesToColor(node1, node2, node3)

  def singleValueToColor: Gen[SingleValueToColor] = singleValueToColor(MaxRecursionLevel)
  private def singleValueToColor(maxDepth: Int): Gen[SingleValueToColor] = for {
    node <- ValueNodeGenerators.randomValueNode(maxDepth)
  } yield SingleValueToColor(node)

  def clampColor: Gen[ClampColor] = clampColor(MaxRecursionLevel)
  private def clampColor(maxDepth: Int): Gen[ClampColor] = for {
    node <- randomColorNode(maxDepth)
    minValue <- arbitrary[Double]
    maxValue <- Gen.choose(minValue,Double.MaxValue)
  } yield ClampColor(node, minValue, maxValue)

  def modColor: Gen[ModColor] = modColor(MaxRecursionLevel)
  private def modColor(maxDepth: Int): Gen[ModColor] = for {
    node <-  randomColorNode(maxDepth)
    mod <- Generators.positiveDouble
  } yield ModColor(node, mod)

  def addColor: Gen[AddColor] = addColor(MaxRecursionLevel)
  private def addColor(maxDepth: Int): Gen[AddColor] = for {
    node1 <- randomColorNode(maxDepth)
    node2 <- randomColorNode(maxDepth)
  } yield AddColor(node1,node2)

  def subtractColor: Gen[SubtractColor] = subtractColor(MaxRecursionLevel)
  private def subtractColor(maxDepth: Int): Gen[SubtractColor] = for {
    node1 <- randomColorNode(maxDepth)
    node2 <- randomColorNode(maxDepth)
  } yield SubtractColor(node1, node2)

  def multiplyColor: Gen[MultiplyColor] = multiplyColor(MaxRecursionLevel)
  private def multiplyColor(maxDepth: Int): Gen[MultiplyColor] = for {
    node1 <- randomColorNode(maxDepth)
    node2 <- randomColorNode(maxDepth)
  } yield MultiplyColor(node1, node2)

  def mixColor: Gen[MixColor] = mixColor(MaxRecursionLevel)
  private def mixColor(maxDepth: Int): Gen[MixColor] = for {
    node1 <- randomColorNode(maxDepth)
    node2 <- randomColorNode(maxDepth)
    mixValue <- ValueNodeGenerators.randomValueNode(maxDepth)
  } yield MixColor(node1, node2, mixValue)

  def terminalValueNode: Gen[ColorNode] =
    Gen.oneOf(constantColor, checkerColor, pointFallColor)

  def randomColorNode: Gen[ColorNode] = randomColorNode(MaxRecursionLevel)
  def randomColorNode(maxLevel: Int): Gen[ColorNode] = {
    if (maxLevel <= 0)
      terminalValueNode
    else {
      val level = maxLevel - 1
      Gen.oneOf(constantColor,
        checkerColor,
        pointFallColor,
        vectorToColor(level),
        valuesToColor(level),
        singleValueToColor(level),
        clampColor(level),
        modColor(level),
        addColor(level),
        subtractColor(level),
        multiplyColor(level)
      )
    }
  }
}
