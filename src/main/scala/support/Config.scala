package support

final case class Config(
    out: String = "",
    in: String = "",
    supersampling: Int = 1,
    adaptivesupersampling: Int = 1,
    verbose: Boolean = false,
    debug: Boolean = false,
    kwargs: Map[String, String] = Map()
)
