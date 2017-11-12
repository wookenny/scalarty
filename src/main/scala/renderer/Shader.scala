package renderer

import color.RGB
import com.typesafe.scalalogging.LazyLogging
import lightning.LightSource
import math.Ray
import math.breeze.VectorBreeze3._
import math.breeze.VectorBreeze3
import support.Config



case class LightSample(light: LightSource, weight: Double, position: VectorBreeze3)

case class Shader(renderer: Renderer)(implicit config: Config) extends LazyLogging {

  import Shader._

  def shadowRay(position: VectorBreeze3, light: VectorBreeze3): Boolean = {
    val vectorToLight = light - position
    val ray = Ray(position, normalized(vectorToLight))
    renderer.scene.allShapes.intersectionTest(ray, VectorBreeze3.length(vectorToLight))
  }

  def shadeHit(hit: Hit, ray: Ray): RGB = {

    val colorInfo = hit.color
    val baseColor: RGB = colorInfo.color
    val backFace = (hit.normal dot ray.direction) > 0

    //ambient
    val ambientColor = baseColor * colorInfo.ambient

    val visibleLights: Seq[LightSample] =
      if (backFace)
        Seq.empty
      else
        for {
          lightSource <- renderer.scene.lights
          lightSampling = lightSource.sample(config.shadowsampling)
          n = lightSampling.size
          lightSample <- lightSampling if !shadowRay(hit.position, lightSample)
        } yield LightSample(lightSource, 1d / n, lightSample)

    //diffuse
    val diffuseColor = shadeDiffuse(hit, ray, visibleLights)
    val specColor = shadeSpecular(hit, ray, visibleLights)
    val reflectedColor = shadeReflection(hit, ray)
    val refractedColor = shadeRefraction(hit, ray)
    ambientColor + diffuseColor + specColor + refractedColor + reflectedColor
  }

  def shadeRefraction(hit: Hit, ray: Ray): RGB = {
    if (hit.color.refractive > ThresholdRayWeight && ray.depth <= RayDepthRefraction) {
      //TODO configurable ray depth
      ray.refractedAt(hit.position, hit.normal, hit.color.n) match {
        case Some(refractedRay) =>
          renderer.traceRay(refractedRay) * hit.color.refractive
        case None =>
          renderer.traceRay(ray.reflectedAt(hit.position, -hit.normal)) * hit.color.refractive //Total Internal ref
      }
    } else {
      Renderer.backgroundColor
    }
  }

  def shadeReflection(hit: Hit, ray: Ray): RGB = {
    if (hit.color.reflective > ThresholdRayWeight && ray.depth <= RayDepthReflection) //TODO make configurable
      renderer.traceRay(ray.reflectedAt(hit.position, hit.normal)) * hit.color.reflective
    else RGB.BLACK
  }

  def shadeSpecular(hit: Hit,
                            r: Ray,
                            visibleLights: Seq[LightSample]): RGB = {
    visibleLights.map {
      lightSample:LightSample =>
        val (l, weight, pos) = (lightSample.light, lightSample.weight, lightSample.position )
        val V = -r.direction //towards eye
        val L = normalized(pos - hit.position) // vector pointing towards light
        val R = V - (hit.normal * ((V dot hit.normal) *2)) //reflected ray
        l.color * Math.pow(Math.max(-(R dot L), 0), hit.color.shininess) *
          hit.color.spec * l.intensity(hit.position, Some(pos)) * weight //spec does not use color of object
    } match {
      case Seq() => RGB.BLACK
      case list => list.reduce(_ + _)
    }
  }

  def shadeDiffuse(hit: Hit,
                           r: Ray,
                           visibleLights: Seq[LightSample]): RGB = {

    visibleLights.map {
      lightSample =>
        val (l, weight, pos) = (lightSample.light, lightSample.weight, lightSample.position )
        val L = normalized(pos - hit.position) // vector pointing towards light //TODO duplicate calculation
        hit.color.color * Math.max(hit.normal dot L, 0) *
          hit.color.diffuse * l.intensity(hit.position, Some(pos)) * weight //TODO light color?
    } match {
      case Seq() => RGB.BLACK
      case list => list.reduce(_ + _)
    }
  }


}

object Shader{
  val RayDepthReflection = 6
  val RayDepthRefraction = 24
  val ThresholdRayWeight = 0.01
}
