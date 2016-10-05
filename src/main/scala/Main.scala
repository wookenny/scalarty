import java.io.File
import play.api.libs.json.Json

case class Config(out: String = "render.png",
                  in: File = new File("."),
                  supersampling: Int = 1,
                  verbose: Boolean = false,
                  debug: Boolean = false,
                  kwargs: Map[String,String] = Map())

object Main {

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("scalarty", "0.0.1")

    opt[File]('i', "in")/*.required()*/.valueName("<file>"). //make this required
      action( (x, c) => c.copy(in = x) ).
      text("in is a required file property")

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

    val sceneFile = "{}"
    val scene = Json.parse(sceneFile).as[Scene]
    val renderer = new Renderer(scene)
    renderer.render(config)
  }
}


