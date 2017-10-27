import com.fasterxml.jackson.core.JsonParseException
import play.api.libs.json.Json
import renderer.Renderer
import scene.{Scene, SceneDTO}
import support.{Config, SamplingValue}
import support.Implicits.imageWriter

import scala.io.Source._



object Main {

  val parser = new scopt.OptionParser[Config]("scopt") {
    val headText = head("scalarty", "0.0.1")

    val file = opt[String]('i', "in")
      .required()
      .valueName("<file>")
      .action((x, c) => c.copy(in = x))
      .text("scene to be rendered")

    val out = opt[String]('o', "out")
      .required()
      .valueName("<file>")
      .action((x, c) => c.copy(out = x))
      .text("output file for the rendered image. Available file formats: " +
        s"${imageWriter.formats.map(_.toLowerCase).distinct.mkString(", ")}")

    val supersampling = opt[SamplingValue]('s', "supersampling")
      .action((x, c) => c.copy(supersampling = x))
      .text(s"<a:b>  for a² full and b² adaptive supersamples," +
        s" (default value=${Config.DefaultSupersampling})")

    val showBvHLeaves = opt[Unit]("bvh.showleafes")
      .abbr("bvh.l")
      .action((_, c) => c.copy(showBvHLeaves = true))
      .text("show BvH leafes used in obj file constructions")

    val verbose = opt[Unit]("verbose")
      .action((_, c) => c.copy(verbose = true))
      .text("enable verbose behaviour")

    val debug = opt[Unit]("debug")
      .action((_, c) => c.copy(debug = true))
      .text("enable debug logs")

    val shadowSampling = opt[SamplingValue]("shadowsampling")
      .abbr("shs")
      .action((x, c) => c.copy(shadowsampling = x))
      .text(s"<a:b>  for a² full and b² adaptive shadow samples for area light." +
        s" (default value=${Config.DefaultImprovedSupersampling})")


    val helpText = help("help").abbr("h").text("prints this usage text")

    val description = note("Simple raytracer written in scala.\n")
  }

  def main(args: Array[String]) {
    parser.parse(args, Config()) match {
      case Some(config) => main(config) // do stuff
      case None => // arguments are bad, error message will be displayed
    }

  }

  def main(implicit config: Config): Unit = {
    val sceneFile: String = fromFile(config.in).getLines.mkString
    try {
      val scene: Scene = Scene.fromDTO(Json.parse(sceneFile).as[SceneDTO])
      val renderer = new Renderer(scene)
      renderer.startRendering(config)
    } catch {
      case e: JsonParseException =>
        println(s"Could not parse ${config.in}: \n\t${e.getOriginalMessage}")
      case e: IllegalArgumentException =>
        println(s"Could not parse scene ${config.in}: \n\t$e")
    }
  }
}
