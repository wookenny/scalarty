package renderer

import color.RGB
import com.typesafe.scalalogging.LazyLogging
import lightning.LightSource
import material.UnshadedMaterial
import math.{Ray, Vector3}
import support.Config

case class LightSample(light: LightSource, weight: Double, position: Vector3)

case class Shader(renderer: Renderer)(implicit config: Config) extends LazyLogging {

  import Shader._

  def shadowRay(position: Vector3, light: Vector3): Double = {
    val vectorToLight = light - position
    val ray = Ray(position, vectorToLight.normalized)
    renderer.scene.allShapes.lightPercentage(ray, vectorToLight.length)
  }

  def shadeHit(hit: Hit, ray: Ray): (RGB, Double) = {

    val colorInfo: UnshadedMaterial = hit.material
    val baseColor: RGB = colorInfo.color
    val backFace = (hit.normal * ray.direction) > 0

   //beer's law
    val beerslawMultiplier = if(ray.insideMedia){
      val refractedColor = RGB.WHITE - baseColor
      val absorptionCoefficient : RGB = -(refractedColor * colorInfo.absorption * hit.distance)
      absorptionCoefficient.expf
    }else{
      RGB.WHITE
    }

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
          lightSample <- lights
          shadowPercentage = shadowRay(hit.position, lightSample) if shadowPercentage > 0
        } yield LightSample(lightSource, shadowPercentage / lights.length, lightSample)

        (visibleLights, visibleLights.size.toDouble / totalLightSamples)
      }

    //non-ambient parts
    val diffuseColor = if(ray.insideMedia) RGB.BLACK else shadeDiffuse(hit, ray, visibleLightSamples)
    val specColor = shadeSpecular(hit, ray, visibleLightSamples)
    val reflectedColor = shadeReflection(hit, ray)
    val refractedColor = shadeRefraction(hit, ray)
    (beerslawMultiplier.mult(ambientColor + diffuseColor + specColor + refractedColor + reflectedColor),
      shadowPercentage)
  }


  //TODO: Ray -> enters object, copy transmition
  def shadeRefraction(hit: Hit, ray: Ray): RGB = {
    if (hit.material.refractive > ThresholdRayWeight && ray.depth <= RayDepthRefraction)
      ray.refractedAt(hit.position, hit.normal, hit.material.n) match {
        case Some(refractedRay) =>
          renderer.traceRay(refractedRay).color * hit.material.refractive
        case None =>
          renderer
            .traceRay(ray.reflectedAt(hit.position, -hit.normal))
            .color * hit.material.refractive //Total Internal ref
      } else Renderer.BackgroundColor
  }

  def shadeReflection(hit: Hit, ray: Ray): RGB = {
    if (hit.material.reflective > ThresholdRayWeight && ray.depth <= RayDepthReflection) //TODO make configurable
      renderer.traceRay(ray.reflectedAt(hit.position, hit.normal)).color * hit.material.reflective
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
          light.color * Math.pow(Math.max(-(R * L), 0), hit.material.shininess) *
            hit.material.spec * light.intensity(hit.position, Some(position)) * weight //spec does not use color of object
        }.reduce(_ + _)
  }

  def shadeDiffuse(hit: Hit, r: Ray, visibleLights: Seq[LightSample]): RGB = {

    if (visibleLights.isEmpty) RGB.BLACK
    else
      visibleLights
        .map { lightSample =>
          val (light, weight, position) =
            (lightSample.light, lightSample.weight, lightSample.position)
          val L = (position - hit.position).normalized // vector pointing towards light //TODO duplicate calculation
          hit.material.color * hit.material.diffuse  * Math.max(hit.normal * L, 0) *
             light.intensity(hit.position, Some(position)) * weight //TODO light color?
        }
        .reduce(_ + _)
  }

}

object Shader {
  val RayDepthReflection = 6
  val RayDepthRefraction = 24
  val ThresholdRayWeight = 0.01
}
