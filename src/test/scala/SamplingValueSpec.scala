import org.scalacheck.{Gen, Prop}
import org.specs2.{ScalaCheck, Specification}
import support.SamplingValue
import org.scalacheck.Prop.forAll

import scala.util.Try

class SamplingValueSpec extends Specification with ScalaCheck {
  override def is = s2"""
      A SamplingValue should
        initialize without adaptive sampling $initWithoutAdaptiveSampling
        initialize with adaptive sampling $initWithAdaptiveSampling
        failing to initialize $failForWrongAdaptiveValues
        SamplingValue.samplingValueRead.reads
          positive values separated by a colon $readsGeneralValues
          single positive integer as full $readInteger
          single positive integer with colon as full $readIntegerWithColon
          colon with single positive integer as adaptive wwith single full$readColonWithInteger
          empty string as single full path $readEmptyString
    """

  val initWithoutAdaptiveSampling = forAll(Gen.choose(1, 1000)) { full =>
    val sv = SamplingValue(full, 0)
    sv.full == full && sv.adaptive == 0 && !sv.secondPath
  }

  val genWithAdaptivePath = for {
    full <- Gen.choose(1, 1000)
    adaptive <- Gen.choose(full + 1, 2000)
  } yield (full, adaptive)

  val initWithAdaptiveSampling = forAll(genWithAdaptivePath) { tuple =>
    val (full, adaptive) = tuple
    val sv = SamplingValue(full, adaptive)
    sv.full == full && sv.adaptive == adaptive && sv.secondPath
  }

  val failForWrongAdaptiveValues = forAll(Gen.choose(3, 1000)) { full =>
    Try(SamplingValue(full, full - 1)).isFailure
  }

  val readsGeneralValues = forAll(Gen.posNum[Int], Gen.posNum[Int]) { (full, adaptive) =>
    SamplingValue.samplingValueRead.reads(s"$full:$adaptive") == SamplingValue(full max 1,
                                                                               if (adaptive > full)
                                                                                 adaptive
                                                                               else 0)
  }

  val readInteger = forAll(Gen.posNum[Int]) { x: Int =>
    SamplingValue.samplingValueRead.reads(s"$x") == SamplingValue(x max 1, 0)
  }

  val readIntegerWithColon = forAll(Gen.posNum[Int]) { x: Int =>
    SamplingValue.samplingValueRead.reads(s"$x:") == SamplingValue(x max 1, 0)
  }

  val readColonWithInteger = forAll(Gen.posNum[Int]) { x: Int =>
    val expectedSampling = if (x > 1) SamplingValue(1, x) else SamplingValue(1, 0)
    SamplingValue.samplingValueRead.reads(s":$x") == expectedSampling
  }

  def readEmptyString = SamplingValue.samplingValueRead.reads("") === SamplingValue(1, 0)

}
