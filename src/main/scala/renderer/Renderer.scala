package renderer

import color.RGB
import com.typesafe.scalalogging._
import lightning.Light
import math.{Ray, Vector3}
import scene.Scene
import support.Image
import support.Config

import scala.Seq

object Renderer {
  val backgroundColor = RGB.BLACK
}

class Renderer(val scene: Scene) extends LazyLogging {

  def shadowRay(position: Vector3, light: Vector3): Boolean = {
    val vectorToLight = (light - position)
    anyHit(Ray(position, vectorToLight.normalized), vectorToLight.length)
  }

  def shadeHit(hit: Hit, r: Ray): RGB = {

    val colorInfo = hit.color
    val baseColor: RGB = colorInfo.color
    val backFace = (hit.normal * r.direction) > 0

    //ambient
    val ambientColor = baseColor * colorInfo.ambient

    //visible lights
    val visibleLights = scene.lights.filter(l =>
      !backFace && !shadowRay(hit.position, l.position))

    //diffuse
    val diffuseColor   = shadeDiffuse(hit, r, visibleLights)
    val specColor      = shadeSpecular(hit, r, visibleLights)
    val reflectedColor = shadeReflection(hit, r)
    val refractedColor = shadeRefraction(hit, r)
    ambientColor + diffuseColor + specColor + refractedColor + reflectedColor
  }

  def shadeRefraction(hit: Hit, r: Ray): RGB = {
    if (hit.color.refractive > 0.01 && r.depth <= 6) {
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

  def shadeReflection(hit: Hit, r: Ray): RGB = {
    if (hit.color.reflective > 0.01 && r.depth <= 6) //TODO make configurable
      traceRay(r.reflectedAt(hit.position, hit.normal)) * hit.color.reflective
    else RGB.BLACK
  }

  def shadeSpecular(hit: Hit, r: Ray, visibleLights: Seq[Light]): RGB = {
    visibleLights.map { l => {
      val V = r.direction * -1 //towards eye
      val L = (l.position - hit.position).normalized // vector pointing towards light
      val R = V - hit.normal * (V * hit.normal) * 2 //reflected ray
      l.color * Math
        .pow(Math.max(-(R * L), 0), hit.color.shininess)
        .toFloat * hit.color.spec * l.intensity //spec does not use color of object
    }
    } match {
      case Seq() => RGB.BLACK
      case list => list.reduce(_ + _)
    }
  }

  def shadeDiffuse(hit: Hit, r: Ray, visibleLights: Seq[Light]): RGB = {
    visibleLights.map { l => {
      val L = (l.position - hit.position).normalized // vector pointing towards light //TODO duplicate calculation
      hit.color.color * Math
        .max((hit.normal * L), 0) * hit.color.diffuse * l.intensity //TODO light color?
    }
    } match {
      case Seq() => RGB.BLACK
      case list => list.reduce(_ + _)
    }
  }

  def traceRay(r: Ray): RGB =
    getFirstHit(r) match {
      case Some(hit) => shadeHit(hit, r)
      case _ => Renderer.backgroundColor
    }

  def getFirstHit(r: Ray): Option[Hit] =
    scene.shapes.flatMap { s =>
      s intersect r
    } match {
      case Nil => None
      case xs => Some(xs.minBy(_.distance))
    }

  def anyHit(r: Ray, maxDist: Float): Boolean = scene.shapes.exists { s =>
    s.intersect(r, maxDist)
  }

  def startRendering(config: Config) = {
    val start = System.nanoTime()
    logger.info("Starting to trace")

    val image = render(config)

    val now = System.nanoTime()
    logger.info(
      s"Raytracing done in ${(now - start) / (1000f * 1000 * 1000)} seconds") //TODO nice time formatter

    val saved = image.save(config.out)
    if(!saved)
      logger.error(s"Could not save image at ${config.out}")

  }

  def render(config: Config) : Image = {

    val img = new Image((scene.width * scene.ppi).toInt,
                        (scene.height * scene.ppi).toInt)



    logger.info( s"tracing ${config.supersampling}x${config.supersampling} per pixel")

    renderPath(img, config.supersampling, None)

    if(config.adaptivesupersampling > 1 && config.adaptivesupersampling > config.supersampling) {
      //find edges and supersample those
      val edges = img.detectEdges()
      val percentage = 100.0*edges.size/(img.height*img.width)
      logger.info( s"tracing adaptive supersampling for $percentage% of all pixels with sampling ${config.adaptivesupersampling}x${config.adaptivesupersampling}")
      renderPath(img, config.adaptivesupersampling, Some(edges))
    }

    img
  }

  def renderPath(img: Image, supersampling: Int, selection: Option[Set[(Int,Int)]]): Unit = {
    val w: Float = scene.width
    val h: Float = scene.height
    val corner = scene.cameraOrigin + scene.cameraPointing - scene.side * (w / 2) + scene.up * (h / 2)

    val X = img.width
    val Y = img.height

    val iter: Iterator[(Int, Int)] =
      for {
        x <- 0 until img.width iterator;
        y <- 0 until img.height iterator
      } yield (x, y)

    iter.toStream.par foreach {
      case (x: Int, y: Int)  => {
        if(selection==None || selection.get.contains((x,y)) ) {

          val S = supersampling * supersampling
          val colorSum: RGB =
            (for {
              i <- 0 until supersampling
              j <- 0 until supersampling
              shift = (supersampling - 1) / (2f * supersampling)
              rayTarget = (corner
                + scene.side * ((w * (x + i.toFloat / supersampling - shift)) / X)
                - scene.up * (h * (y + j.toFloat / supersampling - shift) / Y))
              rayDir = (rayTarget - scene.cameraOrigin).normalized
              description = s"pixel ($x:$y) sample ${(i) * supersampling + (j + 1)}/$S"
            } yield
              traceRay(Ray(scene.cameraOrigin, rayDir, source = description)).exposureCorrected.gammaCorrected)
              .reduce(_ + _)

          //if (selection == None)
            img.set(x, y, colorSum / S)
          //else
          //  img.set(x, y, RGB.RED)
        }
      }
    }
  }
}
