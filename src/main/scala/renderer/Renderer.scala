package renderer

import java.util.concurrent.atomic.AtomicInteger

import color.RGB
import com.typesafe.scalalogging._
import lightning.LightSource
import math.{Ray, Vector3}
import scene.Scene
import support.{Config, Image}
import support.Implicits.imageWriter
import support.Util._

import scala.Seq
import scala.collection.GenSet
import scala.collection.parallel.mutable.ParArray

object Renderer {
  private val backgroundColor = RGB.BLACK
  private val chunkSize = 10

  //TODO: make configurable
  private val lightSampling = 1
}

class Renderer(val scene: Scene)(implicit config: Config) extends LazyLogging {

  implicit val log: (String) => Unit = s => logger.info(s)
  private val tracedPixels: AtomicInteger = new AtomicInteger(0)

  private def shadowRay(position: Vector3, light: Vector3): Boolean = {
    val vectorToLight = light - position
    anyHit(Ray(position, vectorToLight.normalized), vectorToLight.length)
  }

  private def shadeHit(hit: Hit, r: Ray): RGB = {

    val colorInfo = hit.color
    val baseColor: RGB = colorInfo.color
    val backFace = (hit.normal * r.direction) > 0

    //ambient
    val ambientColor = baseColor * colorInfo.ambient

    val visibleLights : Seq[(LightSource,Double,Vector3)] = if(backFace)
      Seq.empty
    else
      for {
        lightSource  <- scene.lights
        lightSampling  = lightSource.sample(Renderer.lightSampling)
        n = lightSampling.size
        lightSample <- lightSampling if !shadowRay(hit.position, lightSample)
      } yield (lightSource, 1d/n, lightSample)

    //diffuse
    val diffuseColor = shadeDiffuse(hit, r, visibleLights)
    val specColor = shadeSpecular(hit, r, visibleLights)
    val reflectedColor = shadeReflection(hit, r)
    val refractedColor = shadeRefraction(hit, r)
    ambientColor + diffuseColor + specColor + refractedColor + reflectedColor
  }

  private def shadeRefraction(hit: Hit, r: Ray): RGB = {
    if (hit.color.refractive > 0.01 && r.depth <=24) {
      //TODO configurable ray depth
      r.refractedAt(hit.position, hit.normal, hit.color.n) match {
        case Some(refractedRay) =>
          traceRay(refractedRay) * hit.color.refractive
        case None =>
          traceRay(r.reflectedAt(hit.position, -hit.normal)) * hit.color.refractive //Total Internal ref
      }
    } else {
      Renderer.backgroundColor
    }
  }

  private def shadeReflection(hit: Hit, r: Ray): RGB = {
    if (hit.color.reflective > 0.01 && r.depth <= 6) //TODO make configurable
      traceRay(r.reflectedAt(hit.position, hit.normal)) * hit.color.reflective
    else RGB.BLACK
  }

  private def shadeSpecular(hit: Hit, r: Ray, visibleLights: Seq[(LightSource,Double,Vector3)]): RGB = {
    visibleLights.map { case (l:LightSource, weight: Double, pos: Vector3) =>
    {
      val V = r.direction * -1 //towards eye
    val L = (pos - hit.position).normalized // vector pointing towards light
    val R = V - hit.normal * (V * hit.normal) * 2 //reflected ray
      l.color * Math.pow(Math.max(-(R * L), 0), hit.color.shininess)*
        hit.color.spec * l.intensity(hit.position, Some(pos)) * weight //spec does not use color of object
    }
    } match {
      case Seq() => RGB.BLACK
      case list => list.reduce(_ + _)
    }
  }

  private def shadeDiffuse(hit: Hit, r: Ray, visibleLights: Seq[(LightSource,Double,Vector3)]): RGB = {

    visibleLights.map { case (l:LightSource, weight: Double, pos: Vector3) =>
    {
      val L = (pos - hit.position).normalized // vector pointing towards light //TODO duplicate calculation
      hit.color.color * Math.max((hit.normal * L), 0) *
        hit.color.diffuse * l.intensity(hit.position, Some(pos)) * weight//TODO light color?
    }
    } match {
      case Seq() => RGB.BLACK
      case list => list.reduce(_ + _)
    }
  }


  private  def traceRay(r: Ray): RGB =
    getFirstHit(r) match {
      case Some(hit) => shadeHit(hit, r)
      case _ => Renderer.backgroundColor
    }

  private def getFirstHit(r: Ray): Option[Hit] = scene.allShapes.intersect(r)

  private def anyHit(r: Ray, maxDist: Double): Boolean =
    scene.allShapes.intersectionTest(r, maxDist)

  def startRendering(config: Config) = {
    val start = System.nanoTime()
    logger.info("Starting to trace")
    logger.info(s"scene contains ${scene.allShapes.size} shapes")

    val image = time("Rendering scene took") { render(config) }

    val now = System.nanoTime()
    logger.info(
      f"Raytracing done in ${(now - start) / (1000f * 1000 * 1000)}%2.2f seconds") //TODO nice time formatter

    val saved = image.save(config.out)
    if (!saved)
      logger.error(s"Could not save image at ${config.out}")

  }

  def render(config: Config): Image = {

    val img = new Image((scene.width * scene.ppi).toInt,
                        (scene.height * scene.ppi).toInt)

    logger.info(
      s"tracing ${config.supersampling}x${config.supersampling} per pixel")

    renderPath(img, config.supersampling, None)

    if (config.adaptivesupersampling > 1 && config.adaptivesupersampling > config.supersampling) {
      //find edges and supersample those
      val edges = img.detectEdges()
      val percentage = 100.0 * edges.size / (img.height * img.width)
      logger.info(
        f"tracing adaptive supersampling for $percentage%2.1f%% of all pixels with sampling ${config.adaptivesupersampling}x${config.adaptivesupersampling}")
      renderPath(img, config.adaptivesupersampling, Some(edges))
    }

    img
  }

  private def buildRenderChunks(
      img: Image,
      selection: Option[GenSet[(Int, Int)]]): ParArray[Array[(Int, Int)]] = {

    val allPixels: Array[(Int, Int)] =
      (for {
        x <- 0 until img.width
        y <- 0 until img.height
        if None == selection || selection.get.contains((x, y))
      } yield (x, y)).toArray

    val chunks: Array[Array[(Int, Int)]] =
      (for {
        s <- 0 to allPixels.size / Renderer.chunkSize
      } yield
        allPixels.slice(s * Renderer.chunkSize, (s + 1) * Renderer.chunkSize)).toArray

    chunks.par
  }

  private def renderPath(img: Image,
                 supersampling: Int,
                 selection: Option[GenSet[(Int, Int)]]): Unit = {
    val w: Double = scene.width
    val h: Double = scene.height
    val corner = scene.cameraOrigin + scene.cameraPointing - scene.side * (w / 2) + scene.up * (h / 2)

    val X = img.width
    val Y = img.height

    val iter: Iterator[(Int, Int)] = for {
      x <- 0 until img.width iterator;
      y <- 0 until img.height iterator
    } yield (x, y)

    val workChunks = buildRenderChunks(img, selection)

    workChunks foreach { pixels =>
      pixels foreach {
        case (x: Int, y: Int) => {
          val S = supersampling * supersampling
          val colorSum: RGB =
            (for {
              i <- 0 until supersampling
              j <- 0 until supersampling
              shift = (supersampling - 1) / (2.0 * supersampling)
              rayTarget = (corner
                + scene.side * ((w * (x + i.toDouble / supersampling - shift)) / X)
                - scene.up * (h * (y + j.toDouble / supersampling - shift) / Y))
              rayDir = (rayTarget - scene.cameraOrigin).normalized
              description = s"pixel ($x:$y) sample ${(i) * supersampling + (j + 1)}/$S"
            } yield
              traceRay(Ray(scene.cameraOrigin, rayDir, source = description)).exposureCorrected.gammaCorrected)
              .reduce(_ + _)

          img.set(x, y, colorSum / S)
          val status = tracedPixels.incrementAndGet()
          val one_percent = X * Y / 100
          if (status % (5 * one_percent) == 0)
            logger.info(s"traced $status pixels -> ${status / one_percent}%")
        }
      }
    }
  }

}
