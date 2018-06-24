package noise

import org.scalacheck.{Gen, Prop}
import org.specs2.mock.Mockito
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}



case class NoiseTester(noise2: (Double,Double) => Double,
                       noise3: (Double,Double,Double) => Double,
                       noise4: (Double,Double,Double,Double) => Double,
                       range2: (Double,Double),
                       range3: (Double,Double),
                       range4: (Double,Double),
                       noiseSize: Double = 1) extends Noise {
 val seed = 0L
  override  def valueFunction(x: Double, y: Double) = noise2(x,y)
  override  def valueFunction(x: Double, y: Double, z:Double) = noise3(x,y,z)
  override  def valueFunction(x: Double, y: Double, z:Double, w:Double) = noise4(x,y,z,w)
}

class NoiseSpec extends Specification with ScalaCheck with Mockito {

  override def is: SpecStructure = s2"""
      A Noise implementation should
        call eval using the noiseSize as scaling factor $testScaling
        return values in [0,1] when evaluation normalized $testNormalization
    """

  private val scaleAndPositionGen: Gen[(Double, Double, Double, Double, Double)] = for{
    scale <- Gen.choose(1d,100d)
    x <- Gen.choose(1d,1000d)
    y <- Gen.choose(1d,1000d)
    z <- Gen.choose(1d,1000d)
    w <- Gen.choose(1d,1000d)
  } yield (scale,x,y,z,w)

  private def testScaling = Prop.forAll(scaleAndPositionGen){ scaleAndPosition =>
    val (scale,x,y,z,w) = scaleAndPosition
    val noise2 = mock[(Double,Double) => Double]
    val noise3 = mock[(Double,Double,Double) => Double]
    val noise4 = mock[(Double,Double,Double,Double) => Double]
    noise2.apply(anyDouble,anyDouble).returns(0d)
    noise3.apply(anyDouble,anyDouble,anyDouble).returns(0d)
    noise4.apply(anyDouble,anyDouble,anyDouble,anyDouble).returns(0d)


    val range = (0d,1d)
    val noiseScaled = NoiseTester(noise2, noise3, noise4, range, range, range, noiseSize = scale)

    noiseScaled.eval(x*scale,y*scale)
    noiseScaled.eval(x*scale,y*scale,z*scale)
    noiseScaled.eval(x*scale,y*scale,z*scale,w*scale)
    got{
      one(noise2).apply(be ~(x +/- 0.1), be ~(y +/- 0.1))
      one(noise3).apply(be ~(x +/- 0.1), be ~(y +/- 0.1), be ~(z +/- 0.1))
      one(noise4).apply(be ~(x +/- 0.1), be ~(y +/- 0.1), be ~(z +/- 0.1), be ~(w +/- 0.1))
    }
  }

  private val rangeAndValueWithin: Gen[((Double, Double), Double)] = for{
    min <- Gen.choose(-1000d,1000d)
    max <- Gen.choose(min,1000d)
    value <- Gen.choose(min,max)
  } yield ((min,max),value)


  private def testNormalization = Prop.forAll(rangeAndValueWithin){
    x:((Double,Double),Double) =>

     val (range,value) = x

     val noise = NoiseTester(noise2 = (_,_) =>  value,
                             noise3 = (_,_,_) =>  value,
                             noise4 = (_,_,_,_) =>  value,
                             range2 = range,
                             range3 = range,
                             range4 = range)

      (noise.evalNormalized(0,0) should beBetween(0d,1d)) and
        (noise.evalNormalized(0,0,0) should beBetween(0d,1d)) and
          (noise.evalNormalized(0,0,0,0) should beBetween(0d,1d))
  }
}
