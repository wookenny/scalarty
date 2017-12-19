package support

import scopt.Read
import scopt.Read.reads

case class SamplingValue(full:Int, adaptive: Int){
  require(adaptive == 0 || full<adaptive, "adaptive sampling has to be bigger than the full image sampling or disabled")
   lazy val secondPath : Boolean = adaptive > 1 && adaptive > full
}

object SamplingValue{
  implicit val samplingValueRead: Read[SamplingValue] = reads {str =>
    val pattern = "([0-9]*):?([0-9]*)".r
    val pattern(a,b) = str.trim
    val full = if(a.isEmpty) 1 else a.toInt max 1
    val adaptive = if(b.isEmpty) 0 else b.toInt max 0
    SamplingValue(full, if(adaptive<=full) 0 else adaptive)
  }
}

final case class Config(
                         out: String = "",
                         in: String = "",
                         supersampling : SamplingValue = SamplingValue.samplingValueRead.reads(Config.DefaultSupersampling),
                         shadowsampling : SamplingValue= SamplingValue.samplingValueRead.reads(Config.DefaultShadowSupersampling),
                         showBvHLeaves: Boolean = Config.DefaultShowBvhLeaves,
                         verbose: Boolean = Config.DefaultVerbose,
                         debug: Boolean = Config.DefaultDebug,
                         sah : Boolean = Config.DefaultSAH,
                         bvhSplitLimit : Int = Config.DefaultBvhSplitlimit,
                         kwargs: Map[String, String] = Map()
)

object Config {
  val DefaultShowBvhLeaves = false
  val DefaultDebug = false
  val DefaultVerbose = false
  val DefaultSupersampling = "1:0"
  val DefaultShadowSupersampling = "1:0"
  val DefaultSAH = false
  val DefaultBvhSplitlimit = 20
}
