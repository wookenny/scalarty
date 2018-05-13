package material.node

import generators.Generators._
import math.Vector3
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.Specification
import org.scalacheck.Arbitrary.arbitrary


class ValueNodeSpec extends Specification with ScalaCheck {
  def is =
    s2"""
      A ConstantValue should
        return the same value everywhere $testConstantValue

      A CheckerValue should
        return different colors for swapped colors $testCheckerValueSwappedColors
        return different colors for offset of stepsize $testCheckerValueOffset
        return different colors for stepsize shift in x,y,z mult by scaling $testCheckerValueStepsizeOffsetScaled

      A NoiseValue should
        return the same value for same position and seed $testNoiseValueSameInput
        return a different values for different seeds ${testNoiseValueDifferentSeed.pendingUntilFixed("(test noise first and inject it)")}
        return scaled version for bigger size $testNoiseValueDifferentSize
        return value be in range [-1,1] $testNoiseValueRange

      A MultilayerNoiseValue
        return the same value for same position and seed $testMultilayerNoiseValueSameInput
        return a different value for a different seed $testMultilayerNoiseValueDifferentSeed
        return scaled version for bigger size ${testMultilayerNoiseValueDifferentSize.pendingUntilFixed("(adding NoiseValueTest and inject the Noise Function)")}
         return value be in range [-1,1] $testMultilayerNoiseValueRange

      """

  def testConstantValue: Prop = forAll(positiveDouble, vector3) { (value, position) =>
    ConstantValue(value).value(position) shouldEqual value
  }

  def testCheckerValueSwappedColors: Prop = forAll(smallPositiveDouble, vector3) { (stepSize, position) =>
    val (two, four) = (Some(2d), Some(4d))
    val checkerValue = CheckerValue(stepSize, value1 = two, value2 = four)
    val swappedCheckerValue = CheckerValue(stepSize, value1 = four, value2 = two)

    checkerValue.value(position) shouldNotEqual swappedCheckerValue.value(position)
  }

  def testCheckerValueOffset: Prop = forAll(smallPositiveDouble, vector3) { (stepSize, position) =>
    val checkerValue = CheckerValue(stepSize)
    val checkerValueOffsetted = CheckerValue(stepSize, offset = Some(Vector3.ONE * stepSize))

    checkerValue.value(position) shouldNotEqual checkerValueOffsetted.value(position)
  }

  def testCheckerValueStepsizeOffsetScaled: Prop = forAll(smallMultipleOfaQuarter, vector3, smallPositiveIntegerVector3) {
    (stepSize, position, scalingVector) =>

    val checkerValue = CheckerValue(stepSize, scaling = Some(scalingVector))
    val shiftX = position.copy(x=position.x + stepSize/scalingVector.x)
    val shiftY = position.copy(y=position.y + stepSize/scalingVector.y)
    val shiftZ = position.copy(z=position.z + stepSize/scalingVector.z)

    (checkerValue.value(position) shouldNotEqual checkerValue.value(shiftX)) and
      (checkerValue.value(position) shouldNotEqual checkerValue.value(shiftY)) and
      (checkerValue.value(position) shouldNotEqual checkerValue.value(shiftZ))
  }

  def testNoiseValueSameInput: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val noiseValue = NoiseValue(seed)
    noiseValue.value(position) shouldEqual noiseValue.value(position)
  }

  def testNoiseValueDifferentSeed: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val noiseForDifferentSeeds = Range(1,30).map(v => NoiseValue(v + seed).value(position)).toSet
    noiseForDifferentSeeds.size should beGreaterThan(1)
  }

  def testNoiseValueDifferentSize: Prop = forAll(vector3, smallMultipleOfaQuarter) { (position, scaling) =>
    val noiseValue       = NoiseValue(seed = 0L)
    val noiseValueScaled = NoiseValue(seed = 0L, size = scaling)
    noiseValue.value(position) shouldEqual noiseValueScaled.value(position * scaling)
  }

  def testNoiseValueRange: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val noiseValue = NoiseValue(seed).value(position)
    noiseValue should be beBetween(-1d,1d)
  }

  def testMultilayerNoiseValueSameInput: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val multilayerNoiseValue = MultilayerNoiseValue(seed)
    multilayerNoiseValue.value(position) shouldEqual multilayerNoiseValue.value(position)
  }

  def testMultilayerNoiseValueDifferentSeed: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val noiseValue1 = MultilayerNoiseValue(seed)
    val noiseValue2 = MultilayerNoiseValue(seed + 1)
    noiseValue1.value(position) shouldNotEqual noiseValue2.value(position)
  }

  def testMultilayerNoiseValueDifferentSize: Prop = forAll(vector3) { position =>
    val noiseValue       = MultilayerNoiseValue(seed = 0L, size = 1)
    val noiseValueScaled = MultilayerNoiseValue(seed = 0L, size = 2)
    noiseValue.value(position) shouldEqual noiseValueScaled.value(position * 2)
  }

  def testMultilayerNoiseValueRange: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val noiseValue = MultilayerNoiseValue(seed).value(position)
    noiseValue should be beBetween(-1d,1d)
  }
}

/*
stepSize: 1.2, pos: Vector3(-10.0,0.0,-16.0) and scaling: Vector3(1.0,1.0,1.0)
comparing with: Vector3(-8.8,0.0,-16.0), Vector3(-10.0,1.2,-16.0), Vector3(-10.0,0.0,-14.8)

 */