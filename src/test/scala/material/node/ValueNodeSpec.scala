package material.node

import generators.Generators._
import math.Vector3
import noise.{Noise, NoiseTester}
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.Specification
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.mock.Mockito


class ValueNodeSpec extends Specification with ScalaCheck with Mockito{
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
        return value be in range [-1,1] $testNoiseValueRange
        be initialized with seed and size $testNoiseValueInitialization
        call the noise function with correct parameters $testNoiseValueCorrectNoiseCall


      A MultilayerNoiseValue
        return the same value for same position and seed $testMultilayerNoiseValueSameInput
        return a different value for a different seed $testMultilayerNoiseValueDifferentSeed
        be initialized with seed, size and octaves $testMultilayerNoiseValueInitialization
        return scaled version for bigger size $testMultilayerNoiseValueDifferentSize
        return value be in range [-1,1] $testMultilayerNoiseValueRange
        call the noise function with correct parameters $testMultilayeNoiseValueCorrectNoiseCall

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

  def testNoiseValueInitialization: Prop = forAll(arbitrary[Long],arbitrary[Double], vector3) { (seed, size, position) =>
    val genNoise = mock[(Long,Double) => Noise]
    NoiseValue(seed,size)(genNoise)
    there was one(genNoise).apply(seed,size)
  }

  def testNoiseValueCorrectNoiseCall: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val noiseFunction = mock[Noise]
    val genNoise : (Long,Double) => Noise = (_,_) => noiseFunction
    val noiseValue = NoiseValue(seed)(genNoise).value(position)
    there was one(noiseFunction).evalNormalized(position.x,position.y,position.z)
  }

  def testNoiseValueRange: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    NoiseValue(seed).value(position) should be beBetween(-1d,1d)
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



  def testMultilayerNoiseValueDifferentSize: Prop = forAll(vector3,smallPositiveDouble) { (position,scaling) =>

    val noise2 = mock[(Double,Double) => Double]
    val noise3 = mock[(Double,Double,Double) => Double]
    val noise4 = mock[(Double,Double,Double,Double) => Double]
    noise2.apply(anyDouble,anyDouble).returns(0d)
    noise3.apply(anyDouble,anyDouble,anyDouble).returns(0d)
    noise4.apply(anyDouble,anyDouble,anyDouble,anyDouble).returns(0d)


    val range = (0d,1d)
    val noiseScaled: Noise = NoiseTester(noise2, noise3, noise4, range, range, range, noiseSize = scaling)


    val genNoise : (Long,Double) => Noise = (_,_) => noiseScaled
    val noiseValueScaled = MultilayerNoiseValue(seed = 0L, size = scaling)(genNoise)
    val noiseValue       = MultilayerNoiseValue(seed = 0L)(genNoise)


    noiseValue.value(position)
    noiseValueScaled.value(position * scaling)
    got {
      one(noise3).apply(be ~ (position.x +/- 0.1), be ~ (position.y +/- 0.1), be ~ (position.z +/- 0.1))
      one(noise3).apply(be ~ (position.x +/- 0.1), be ~ (position.y +/- 0.1), be ~ (position.z +/- 0.1))
    }

  }

  def testMultilayerNoiseValueRange: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val noiseValue = MultilayerNoiseValue(seed).value(position)
    noiseValue should be beBetween(-1d,1d)
  }

  def testMultilayerNoiseValueInitialization: Prop = forAll(arbitrary[Long], arbitrary[Double], arbitrary[Option[Int]], vector3) { (seed, size, octaves, position) =>
    val genNoise = mock[(Long,Double) => Noise]
    MultilayerNoiseValue(seed,size,octaves)(genNoise)
    there was one(genNoise).apply(seed,size)
  }

  def testMultilayeNoiseValueCorrectNoiseCall: Prop = forAll(arbitrary[Long], vector3) { (seed, position) =>
    val noiseFunction = mock[Noise]
    val genNoise : (Long,Double) => Noise = (_,_) => noiseFunction
    val noiseValue = MultilayerNoiseValue(seed)(genNoise).value(position)
    there was one(noiseFunction).evalNormalized(position.x,position.y,position.z)
  }
}
