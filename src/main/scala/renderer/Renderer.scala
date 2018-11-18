package renderer

import java.util.concurrent.atomic.AtomicInteger

import color.RGB
import com.typesafe.scalalogging._
import math.Ray
import scene.Scene
import support.Implicits.imageWriter
import support.Util._
import support.{Config, Image}

import scala.collection.GenSet

object Renderer {
  val BackgroundColor = RGB.BLACK
  private val ChunkSize = 5

  def abbrevNumber(n: Int) = n match {
    case _ if n < 1000                      => n.toString
    case _ if n < 1000 * 10                 => f"${n / 1000.0}%1.1fk"
    case _ if n < 1000 * 1000               => f"${n / 1000}%3dK"
    case _ if n < 1000 * 1000 * 10          => f"${n / (1000.0 * 1000)}%1.1fM"
    case _ if n < 1000 * 1000 * 1000        => f"${n / (1000 * 1000)}%3dM"
    case _ if n < 1000 * 1000 * 1000 * 10   => f"${n / (1000.0 * 1000 * 1000)}%1.1fG"
    case _ if n < 1000 * 1000 * 1000 * 1000 => f"${n / (1000 * 1000 * 1000)}%3dG"
    case _                                  => n.toString
  }
}

final case class TracingResult(color: RGB, depth: Double, shadow: Double) {
  def +(otherResult: TracingResult): TracingResult =
    TracingResult(color + otherResult.color, depth + otherResult.depth, shadow + otherResult.shadow)
}
object TracingResult {
  val Miss = TracingResult(Renderer.BackgroundColor, Double.PositiveInfinity, 0)
}

case class Renderer(scene: Scene)(implicit config: Config) extends LazyLogging {

  implicit val log: String => Unit = s => logger.info(s)
  private val tracedPixels: AtomicInteger = new AtomicInteger(0)

  private val shader = Shader(this)

  def traceRay(ray: Ray): TracingResult =
    scene.allShapes.intersect(ray) match {
      case Some(hit) =>
        val (color, shadow) = shader.shadeHit(hit, ray)
        TracingResult(color, hit.distance, shadow) //find out this value
      case _ => TracingResult.Miss
    }

  def startRendering(config: Config): Unit = {

    time("Parsing scene took") {
      logger.info("Starting to trace")
      logger.info(s"scene contains ${scene.allShapes.size} shapes")
    }

    val image = time("Rendering scene took") { render(config) }

    if (!image.save(config.out))
      logger.error(s"Could not save image at ${config.out}")

  }

  def render(config: Config): Image = {

    implicit val img: Image =
      new Image((scene.width * scene.ppi).toInt, (scene.height * scene.ppi).toInt)

    logger.info(
      s"tracing ${config.supersampling.full}x${config.supersampling.full} per pixel " +
        s"with shadows sampled ${config.shadowsampling.full}x${config.shadowsampling.full}"
    )

    renderPath(img, config.supersampling.full, config.shadowsampling.full, None)

    logger.info("first path done.")

    val edges =
      if (config.supersampling.secondPath) img.detectEdges()
      else GenSet.empty[(Int, Int)]

    val shadowEdges = erode(
      if (config.shadowsampling.secondPath) img.detectShadowEdges()
      else GenSet.empty[(Int, Int)],
      3
    )

    renderSubset(
      edges.intersect(shadowEdges),
      config.supersampling.adaptive,
      config.shadowsampling.adaptive
    )
    renderSubset(edges.diff(shadowEdges), supersampling = config.supersampling.adaptive)
    renderSubset(shadowEdges.diff(edges), shadowsampling = config.shadowsampling.adaptive)

    logger.info("rendering done.")
    img
  }

  def erode(pixels: GenSet[(Int, Int)], strength: Int = 1)(
      implicit img: Image
  ): GenSet[(Int, Int)] =
    (for {
      (x, y) <- pixels.toStream
      i <- -strength to strength if (x + i) >= 0 && (x + i) < img.width
      j <- -strength to strength if (y + j) >= 0 && (y + j) < img.height
    } yield (x + i, y + j)).toSet

  def renderSubset(
      pixels: GenSet[(Int, Int)],
      supersampling: Int = config.supersampling.full,
      shadowsampling: Int = config.shadowsampling.full
  )(implicit img: Image) {
    if (pixels.nonEmpty) {
      val percentage = 100 * pixels.size.toFloat / (img.height * img.width)
      logger.info(
        f"tracing for $percentage%2.1f%% of all pixels" +
          f" with sampling $supersampling and shadow sampling $shadowsampling"
      )
      renderPath(img, supersampling, shadowsampling, Some(pixels))
    }
  }

  def buildRenderChunks(
      img: Image,
      selection: Option[GenSet[(Int, Int)]]
  ): Array[Array[(Int, Int)]] = {

    val allPixels: Array[(Int, Int)] =
      (for {
        x <- 0 until img.width
        y <- 0 until img.height
        if selection.isEmpty || selection.get.contains((x, y))
      } yield (x, y)).toArray

    (for {
      s <- 0 to allPixels.length / Renderer.ChunkSize
    } yield allPixels.slice(s * Renderer.ChunkSize, (s + 1) * Renderer.ChunkSize)).toArray

  }

  def renderPath(
      img: Image,
      supersampling: Int,
      shadowsampling: Int,
      selection: Option[GenSet[(Int, Int)]]
  ): Unit = {
    val w: Double = scene.width
    val h: Double = scene.height
    val corner = scene.cameraOrigin + scene.cameraPointing - scene.side * (w / 2) + scene.up * (h / 2)

    val X = img.width
    val Y = img.height
    val one_percent = X * Y / 100

    val iter: Iterator[(Int, Int)] = for {
      x <- 0 until img.width iterator;
      y <- 0 until img.height iterator
    } yield (x, y)

    val workChunks = buildRenderChunks(img, selection)

    workChunks.par foreach { pixels =>
      pixels foreach {
        case (x: Int, y: Int) =>
          val S = supersampling * supersampling
          val tracedSum: TracingResult = (for {
            i <- 0 until supersampling
            j <- 0 until supersampling
            shift = (supersampling - 1) / (2.0 * supersampling)
            rayTarget = (corner
              + scene.side * ((w * (x + i.toDouble / supersampling - shift)) / X)
              - scene.up * (h * (y + j.toDouble / supersampling - shift) / Y))
            rayDir = (rayTarget - scene.cameraOrigin).normalized
            description = s"pixel ($x:$y) sample ${i * supersampling + (j + 1)}/$S"
          } yield
            traceRay(Ray(scene.cameraOrigin, rayDir, source = description)) match {
              case TracingResult(color, d, s) =>
                TracingResult(color.exposureCorrected.gammaCorrected, d, s)
            })
            .reduce(_ + _)

          img.set(x, y, tracedSum.color / S, tracedSum.depth / S, tracedSum.shadow / S)
          val status = tracedPixels.incrementAndGet()

          if (status % (5 * one_percent) == 0)
            logger.info(
              f"traced ${Renderer.abbrevNumber(status)} pixels -> ${status / one_percent}%3d%%"
            )
      }
    }
  }

}
