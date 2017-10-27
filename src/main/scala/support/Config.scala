package support

import scopt.Read
import scopt.Read.reads

case class SamplingValue(full:Int, adaptive: Int){
  require(adaptive == 1 || full<adaptive, "adaptive sampling has to be bigger than the full image sampling or disabled")
   lazy val secondPath : Boolean = adaptive > 1 && adaptive > full
}

object SamplingValue{
  implicit val samplingValueRead: Read[SamplingValue] = reads {str =>
    val pattern = "([0-9]*):?([0-9]*)".r
    val pattern(a,b) = str.trim
    val full = if(a.isEmpty) 1 else a.toInt max 1
    val adaptive = if(b.isEmpty) 1 else b.toInt max 1
    SamplingValue(full, if(adaptive<=full) 1 else adaptive)
  }
}

final case class Config(
    out: String = "",
    in: String = "",
    supersampling : SamplingValue = SamplingValue.samplingValueRead.reads(Config.DefaultSupersampling),
    shadowsampling : SamplingValue= SamplingValue.samplingValueRead.reads(Config.DefaultImprovedSupersampling),
    showBvHLeaves: Boolean = Config.DefaultShowBvHLeaves,
    verbose: Boolean = Config.DefaultVerbose,
    debug: Boolean = Config.DefaultDebug,
    kwargs: Map[String, String] = Map()
)

object Config {
  val DefaultShowBvHLeaves = false
  val DefaultDebug = false
  val DefaultVerbose = false
  val DefaultSupersampling = "1:1"
  val DefaultImprovedSupersampling = "1:1"
}
