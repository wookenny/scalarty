package renderer

import color.RGB
import com.typesafe.scalalogging.LazyLogging
import lightning.LightSource
import math.{Ray, Vector3}
import support.Config

case class LightSample(light: LightSource, weight: Double, position: Vector3)

case class Shader(renderer: Renderer)(implicit config: Config) extends LazyLogging {

  import Shader._

  def shadowRay(position: Vector3, light: Vector3): Boolean = {
    val vectorToLight = light - position
    val ray = Ray(position, vectorToLight.normalized)
    renderer.scene.allShapes.intersectionTest(ray, vectorToLight.length)
  }

  def shadeHit(hit: Hit, ray: Ray): (RGB, Double) = {

    val colorInfo = hit.color
    val baseColor: RGB = colorInfo.color
    val backFace = (hit.normal * ray.direction) > 0

    //ambient
    val ambientColor = baseColor * colorInfo.ambient

    val (visibleLightSamples, shadowPercentage): (Seq[LightSample], Double) =
      if (backFace)
        (Seq.empty, 0)
      else {
        val lightSamples: Seq[(Seq[Vector3], LightSource)] = for {
          lightSource <- renderer.scene.lights
        } yield (lightSource.sample(config.shadowsampling.full), lightSource)

        val totalLightSamples: Int = lightSamples.map(_._1.size).sum

        val visibleLights = for {
          (lights, lightSource) <- lightSamples
          lightSample <- lights if !shadowRay(hit.position, lightSample)
        } yield LightSample(lightSource, 1d / lights.length, lightSample)

        (visibleLights, visibleLights.size.toDouble / totalLightSamples)
      }

    //diffuse
    val diffuseColor = shadeDiffuse(hit, ray, visibleLightSamples)
    val specColor = shadeSpecular(hit, ray, visibleLightSamples)
    val reflectedColor = shadeReflection(hit, ray)
    val refractedColor = shadeRefraction(hit, ray)
    (ambientColor + diffuseColor + specColor + refractedColor + reflectedColor, shadowPercentage)
  }

  def shadeRefraction(hit: Hit, ray: Ray): RGB = {
    if (hit.color.refractive > ThresholdRayWeight && ray.depth <= RayDepthRefraction)
      ray.refractedAt(hit.position, hit.normal, hit.color.n) match {
        case Some(refractedRay) =>
          renderer.traceRay(refractedRay).color * hit.color.refractive
        case None =>
          renderer
            .traceRay(ray.reflectedAt(hit.position, -hit.normal))
            .color * hit.color.refractive //Total Internal ref
      } else Renderer.BackgroundColor
  }

  def shadeReflection(hit: Hit, ray: Ray): RGB = {
    if (hit.color.reflective > ThresholdRayWeight && ray.depth <= RayDepthReflection) //TODO make configurable
      renderer.traceRay(ray.reflectedAt(hit.position, hit.normal)).color * hit.color.reflective
    else RGB.BLACK
  }

  def shadeSpecular(hit: Hit, r: Ray, visibleLights: Seq[LightSample]): RGB = {
    if (visibleLights.isEmpty)
      RGB.BLACK
    else
      visibleLights
        .map { lightSample: LightSample =>
          val (light, weight, position) =
            (lightSample.light, lightSample.weight, lightSample.position)
          val V = r.direction * -1 //towards eye
          val L = (position - hit.position).normalized // vector pointing towards light
          val R = V - hit.normal * (V * hit.normal) * 2 //reflected ray
          light.color * Math.pow(Math.max(-(R * L), 0), hit.color.shininess) *
            hit.color.spec * light.intensity(hit.position, Some(position)) * weight //spec does not use color of object
        }
        .reduce(_ + _)
  }

  def shadeDiffuse(hit: Hit, r: Ray, visibleLights: Seq[LightSample]): RGB = {

    if (visibleLights.isEmpty) RGB.BLACK
    else
      visibleLights
        .map { lightSample =>
          val (light, weight, position) =
            (lightSample.light, lightSample.weight, lightSample.position)
          val L = (position - hit.position).normalized // vector pointing towards light //TODO duplicate calculation
          hit.color.color * Math.max(hit.normal * L, 0) *
            hit.color.diffuse * light.intensity(hit.position, Some(position)) * weight //TODO light color?
        }
        .reduce(_ + _)
  }

}

object Shader {
  val RayDepthReflection = 6
  val RayDepthRefraction = 24
  val ThresholdRayWeight = 0.01
}
