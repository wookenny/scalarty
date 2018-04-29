package generators.nodes

import generators.Generators.{positiveDouble, vector3}
import material.node._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object ValueNodeGenerators {

  val MaxRecursionLevel = 3

  def constantValue: Gen[ConstantValue] =
    for {
      value <- arbitrary[Double]
    } yield ConstantValue(value)

  def checkerValue: Gen[CheckerValue] =
    for {
      stepSize <- arbitrary[Double]
      offset <- Gen.option(vector3)
      scaling <- Gen.option(vector3)
      value1 <- Gen.option(arbitrary[Double])
      value2 <- Gen.option(arbitrary[Double])

    } yield CheckerValue(stepSize, offset, scaling, value1, value2)

  def noiseValue: Gen[NoiseValue] =
    for {
      seed <- arbitrary[Long]
      size <- arbitrary[Double]
    } yield NoiseValue(seed, size)

  def multilayerNoiseValue: Gen[MultilayerNoiseValue] =
    for {
      seed <- arbitrary[Long]
      size <- arbitrary[Double]
      octaves <- Gen.option(arbitrary[Int])
    } yield MultilayerNoiseValue(seed, size, octaves)

  /* Recursive node types */

  def clampValue: Gen[ClampValue] = clampValue(MaxRecursionLevel)
  private def clampValue(maxDepth: Int): Gen[ClampValue] =
    for {
      node <- randomValueNode(maxDepth)
      minValue <- arbitrary[Double]
      maxValue <- arbitrary[Double]
    } yield ClampValue(node, minValue, maxValue)

  def modValue: Gen[ModValue] = modValue(MaxRecursionLevel)
  private def modValue(maxDepth: Int): Gen[ModValue] =
    for {
      node <- randomValueNode(maxDepth)
      mod <- positiveDouble
    } yield ModValue(node, mod)

  def addValues: Gen[AddValues] = addValues(MaxRecursionLevel)
  private def addValues(maxDepth: Int): Gen[AddValues] =
    for {
      node1 <- randomValueNode(maxDepth)
      node2 <- randomValueNode(maxDepth)
    } yield AddValues(node1, node2)

  def subtractValues: Gen[SubtractValues] = subtractValues(MaxRecursionLevel)
  private def subtractValues(maxDepth: Int): Gen[SubtractValues] =
    for {
      node1 <- randomValueNode(maxDepth)
      node2 <- randomValueNode(maxDepth)
    } yield SubtractValues(node1, node2)

  def multiplyValues: Gen[MultiplyValues] = multiplyValues(MaxRecursionLevel)
  private def multiplyValues(maxDepth: Int): Gen[MultiplyValues] =
    for {
      node1 <- randomValueNode(maxDepth)
      node2 <- randomValueNode(maxDepth)
    } yield MultiplyValues(node1, node2)

  def mixValues: Gen[MixValues] = mixValues(MaxRecursionLevel)
  private def mixValues(maxDepth: Int): Gen[MixValues] =
    for {
      node1 <- randomValueNode(maxDepth)
      node2 <- randomValueNode(maxDepth)
      mixValue <- randomValueNode(maxDepth)
    } yield MixValues(node1, node2, mixValue)

  def terminalValueNode: Gen[ValueNode] =
    Gen.oneOf(constantValue, checkerValue, noiseValue, multilayerNoiseValue)

  def randomValueNode: Gen[ValueNode] = randomValueNode(MaxRecursionLevel)
  def randomValueNode(maxLevel: Int): Gen[ValueNode] = {
    if (maxLevel <= 0)
      terminalValueNode
    else {
      val level = maxLevel - 1
      Gen.oneOf(constantValue,
        checkerValue,
        noiseValue,
        multilayerNoiseValue,
        clampValue(level),
        addValues(level),
        subtractValues(level),
        multiplyValues(level),
        mixValues(level))
    }
  }

}
