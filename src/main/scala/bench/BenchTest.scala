package bench

import io.circe.Error
import io.circe.parser.decode
import io.circe.generic.auto._
import renderer.Renderer
import support.Config

import scala.concurrent.duration.Duration
import scala.io.Source.fromFile
import scene.{Scene, SceneDTO}

object BenchTest {
  def main(args: Array[String]): Unit = {

    val config = Config(in = "./scenes/cornell.json")
    val tests = Seq(config.copy(out = "test1.png"),
                    config.copy(out = "test2.png", sah = true),
                    config.copy(out = "test1.png"),
                    config.copy(out = "test2.png", sah = true))

    val runningTimes: Seq[Duration] = tests flatMap render
    val scaledRunningTimes = runningTimes.map(100d * _.toMillis / runningTimes.min.toMillis)

    println(s"Running times: ${runningTimes.map(_.toMillis).mkString("", " ms ,", " ms")}")
    println(s"Relative times: ${scaledRunningTimes.map(_.toInt).mkString(",")}")

  }

  def render(config: Config): Option[Duration] = {
    val sceneFile: String = fromFile(config.in).getLines.mkString
    val sceneEither: Either[Error, SceneDTO] = decode[SceneDTO](sceneFile)
    implicit val c = config
    sceneEither match {
      case Right(scene) => Some(timed(Renderer(Scene.fromDTO(scene)).startRendering(c))._2)
      case Left(error)  => None
    }
  }

  def timed[A](block: => A): (A, Duration) = {
    val t = System.nanoTime()
    val result = block
    val time = Duration.fromNanos(System.nanoTime() - t)
    (result, time)
  }
}
