package support

final case class Config(
    out: String = "",
    in: String  = "",
    supersampling: Int = Config.DefaultSupersampling,
    adaptivesupersampling: Int = Config.DefaultAdaptiveSupersampling,
    shadowsampling: Int = Config.DefaultShadowSampling,
    showBvHLeaves : Boolean =    Config.DefaultShowBvHLeaves,
    verbose: Boolean = Config.DefaultVerbose,
    debug: Boolean = Config.DefaultDebug,
    kwargs: Map[String, String] = Map()
)

object Config {
  val DefaultShowBvHLeaves = false
  val DefaultDebug = false
  val DefaultVerbose = false
  val DefaultSupersampling = 1
  val DefaultAdaptiveSupersampling = 1
  val DefaultShadowSampling = 1
}