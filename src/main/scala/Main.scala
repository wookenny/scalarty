import play.api.libs.json.Json
import scala.io.Source._

case class Config(out: String = "",
                  in: String = "",
                  supersampling: Int = 1,
                  verbose: Boolean = false,
                  debug: Boolean = false,
                  kwargs: Map[String,String] = Map())

object Main {

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("scalarty", "0.0.1")

    opt[String]('i', "in").required().valueName("<file>").
      action( (x, c) => c.copy(in = x) ).
      text("scene to be rendered")

    opt[String]('o', "out").required().valueName("<file>").
      action( (x, c) => c.copy(out = x) ).
      text("out is a required file for the rendered image")

    opt[Int]('s', "supersampling").action( (x, c) =>
       c.copy(supersampling = x) ).text("foo is an integer property")

    opt[Unit]("verbose").action( (_, c) =>
      c.copy(verbose = true) ).text("verbose is a flag")

    opt[Unit]("debug").hidden().action( (_, c) =>
      c.copy(debug = true) ).text("this option is hidden in the usage text")

    help("help").text("prints this usage text")


    note("Simple raytracer written in scala.\n")

  }


  def main(args: Array[String]) {

    //val renderer = new Renderer
    //renderer.render("/home/torsten/Desktop/image.png")
     // parser.parse returns Option[C]
    // parser.parse returns Option[C]
    parser.parse(args, Config()) match {
      case Some(config) => main(config)// do stuff
      case None =>   // arguments are bad, error message will have been displayed
    }

  }

  def main(config: Config): Unit = {


    val sceneFile : String = fromFile(config.in).getLines.mkString
    val scene = Json.parse(sceneFile).as[Scene]
    val renderer = new Renderer(scene)
    renderer.render(config)
  }
}


