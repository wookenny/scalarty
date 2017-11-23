import bounding.ShapeSeq
import color.RGB
import color.RGB._
import material.UnshadedColor
import math.breeze.VectorBreeze3._
import math.{Ray, Sphere}
import org.specs2.Specification
import org.specs2.mock.Mockito
import renderer.{Hit, Renderer, Shader}
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

    """

  implicit val config: Config = Config()

  val mockedScene = mock[Scene]
  val mockedRenderer = mock[Renderer]
  val shader = Shader(mockedRenderer)
  mockedRenderer.scene returns mockedScene

  val unshadedColor =  UnshadedColor(color = CYAN,
                                     ambient = 0.2,
                                     diffuse = 0.4,
                                     spec = 0.2,
                                     reflective = 0.1,
                                     refractive = 0.1,
                                     n = 1.33,
                                     shininess = 16)

  val hit = Hit(0.2, Y, ONE,unshadedColor)
  val ray = Ray(ZERO,Z)

  val testShadowRayPositive = {
    val shapes = Seq( Sphere( ONE * 10d, 1.2), Sphere(ONE * 0.3, 0.1))
    mockedScene.allShapes returns ShapeSeq(shapes)
    shader.shadowRay(position = ZERO, light = ONE) should beTrue
  }

  val testShadowRayNegative = {
    val shapes = Seq( Sphere( ONE * 10d, 1.2), Sphere(ONE * -0.5, 0.1))
    mockedScene.allShapes returns ShapeSeq(shapes)
    shader.shadowRay(position = ZERO, light = ONE) should beFalse
  }

  val testShadeReflectionStopping = {
    val nonReflectiveColor = unshadedColor.copy(reflective = 0)

    (shader.shadeReflection(hit, ray.copy(depth = Shader.RayDepthReflection + 1)) should be equalTo BLACK) and
      (shader.shadeReflection(hit.copy(color = nonReflectiveColor), ray) should be equalTo BLACK)
  }

  val testShadeReflection = {
    val color = RGB(0.2,0.6,0.1)
    mockedRenderer.traceRay(ray.reflectedAt(hit.position, hit.normal)) returns color
    shader.shadeReflection(hit, ray) should be equalTo (color * 0.1)
  }
}
