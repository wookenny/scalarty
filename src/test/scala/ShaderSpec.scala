import bounding.ShapeSeq
import color.RGB
import color.RGB._
import lightning.PointLight
import material.UnshadedMaterial
import math.{Ray, Sphere, Vector3}
import org.specs2.Specification
import org.specs2.mock.Mockito
import renderer._
import scene.Scene
import support.Config

class ShaderSpec extends Specification with Mockito {

  override def is = s2"""
      A Shader should
        send shadow rays
          positively $testShadowRayPositive
          negatively $testShadowRayNegative
        when shadeReflection
            should stop if above RayDepth or below threshold weight $testShadeReflectionStopping
            should trace and apply factor $testShadeReflection
        when shadeDiffuse
            should return black if there is no visible light $testShadeDiffuseNoLights
            should correct color for multiple lights $testShadeDiffuse

    """
  case class ColorToCompare(color: RGB, delta: Double = 0.01) {

    def withDelta(d: Double) = this.copy(delta = d)

    def shouldBeSimilarTo(color2: RGB) = {
      def colorToSeq(color: RGB) = Seq(color.red, color.green, color.blue)

      val zippedColorComponents = colorToSeq(color).zip(colorToSeq(color2))
      foreach(zippedColorComponents) { case (a: Double, b: Double) => a must be ~ (b +/- delta) }
    }
  }

  implicit def colorToCompare(color: RGB) = ColorToCompare(color)

  implicit val config: Config = Config()

  val mockedScene = mock[Scene]
  val mockedRenderer = mock[Renderer]
  val shader = Shader(mockedRenderer)
  mockedRenderer.scene returns mockedScene

  val unshadedColor = UnshadedMaterial(color = CYAN,
                                    ambient = 0.2,
                                    diffuse = 0.4,
                                    spec = 0.2,
                                    reflective = 0.1,
                                    refractive = 0.1,
                                    n = 1.33,
                                    shininess = 16)

  val hit = Hit(0.2, Vector3.Y, Vector3.ONE, unshadedColor)
  val ray = Ray(Vector3.ZERO, Vector3.Z)

  val testShadowRayPositive = {
    val shapes = Seq(Sphere(Vector3.ONE * 10, 1.2), Sphere(Vector3.ONE * 0.3, 0.1))
    mockedScene.allShapes returns ShapeSeq(shapes)
    shader.shadowRay(position = Vector3.ZERO, light = Vector3.ONE) should beEqualTo(0d)
  }

  val testShadowRayNegative = {
    val shapes = Seq(Sphere(Vector3.ONE * 10, 1.2), Sphere(Vector3.ONE * -0.5, 0.1))
    mockedScene.allShapes returns ShapeSeq(shapes)
    shader.shadowRay(position = Vector3.ZERO, light = Vector3.ONE) should beEqualTo(1d)
  }

  val testShadeReflectionStopping = {
    val nonReflectiveColor = unshadedColor.copy(reflective = 0)

    (shader.shadeReflection(hit, ray.copy(depth = Shader.RayDepthReflection + 1)) should be equalTo BLACK) and
      (shader.shadeReflection(hit.copy(material = nonReflectiveColor), ray) should be equalTo BLACK)
  }

  val testShadeReflection = {
    val color = RGB(0.2, 0.6, 0.1)
    mockedRenderer.traceRay(ray.reflectedAt(hit.position, hit.normal)) returns TracingResult(color,
                                                                                             0,
                                                                                             0)
    shader.shadeReflection(hit, ray) should be equalTo (color * 0.1)
  }

  val testShadeDiffuseNoLights = {
    val lights = Seq.empty[LightSample]
    shader.shadeDiffuse(hit, ray, lights) should be equalTo RGB.BLACK
  }

  val testShadeDiffuse = {
    val sample1 =
      LightSample(PointLight(Vector3.ONE, RGB.RED, power = 2), weight = 1, position = Vector3.ONE)
    val sample2 =
      LightSample(PointLight(Vector3.ONE, RGB.RED, power = 2), weight = 1, position = Vector3.ONE)

    val lights = Seq(sample1, sample2)
    val color = shader.shadeDiffuse(hit, ray, lights)
    color shouldBeSimilarTo RGB(0, 0.653, 0.653) //TODO: Why these values?

  }

}
