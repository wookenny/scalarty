import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import renderer.Renderer
import scene.{Scene, SceneDTO}
import support.{Config, SamplingValue}
import support.Implicits.imageWriter

import scala.io.Source._
import scala.util.{Failure, Success, Try}

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
      .text(
        "output file for the rendered image. Available file formats: " +
          s"${imageWriter.formats.map(_.toLowerCase).distinct.mkString(", ")}"
      )

    val supersampling = opt[SamplingValue]('s', "supersampling")
      .action((x, c) => c.copy(supersampling = x))
      .valueName("<a:b>")
      .text(
        s" for a² full and b² adaptive supersamples," +
          s" (default value=${Config.DefaultSupersampling})"
      )

    val showBvHLeaves = opt[Unit]("bvh.showleaves")
      .abbr("bvh.l")
      .action((_, c) => c.copy(showBvHLeaves = true))
      .text("show BvH leaves used in obj file constructions")

    val verbose = opt[Unit]("verbose")
      .action((_, c) => c.copy(verbose = true))
      .text("enable verbose behaviour")

    val debug = opt[Unit]("debug")
      .action((_, c) => c.copy(debug = true))
      .text("enable debug logs")

    val shadowSampling = opt[SamplingValue]("shadowsampling")
      .abbr("shs")
      .valueName("<a:b>")
      .action((x, c) => c.copy(shadowsampling = x))
      .text(
        s" for a² full and b² adaptive shadow samples for area light." +
          s" (default value=${Config.DefaultShadowSupersampling})"
      )

    val sah = opt[Unit]("nosah")
      .action((_, c) => c.copy(sah = false))
      .text("disable SAH to construct BVHs")

    val bvhMin = opt[Int]("bvh.splitlimit")
      .abbr("bvh.sl")
      .text("set the limit at which the split into smaller subsets stops")
      .action((x, c) => c.copy(bvhSplitLimit = x))

    val helpText = help("help").abbr("h").text("prints this usage text")

    val description = note("Simple raytracer written in scala.\n")
  }

  def main(args: Array[String]) = parser.parse(args, Config()) match {
      case Some(config) => run(config) // do stuff
      case None         => // arguments are bad, error message will be displayed
    }


  def run(implicit config: Config): Unit = {
    val sceneFile: Either[String,String] =
      Try(fromFile(config.in)) match {
        case Success(value) =>
              Right(value.getLines
                         .map(_.replaceAll("//.*", ""))
                         .mkString
                         .replaceAll("/\\*.*\\*/", "")
              )
        case Failure(t) =>  Left(t.getMessage)
      }

    val sceneEither: Either[String, SceneDTO] = sceneFile.flatMap( decode[SceneDTO](_)).left.map(_.toString)

    sceneEither match {
      case Right(scene) => Renderer(Scene.fromDTO(scene, config.in)).startRendering(config)
      case Left(error)  => println(s"ERROR: Rendering ${config.in} failed:\n$error")
    }

  }
}
