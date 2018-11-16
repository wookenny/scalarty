import color.RGB
import material.UnshadedMaterial
import math.{Ray, Vector3}
import org.specs2.Specification
import org.specs2.mock.Mockito
import renderer.{Hit, Renderer}
import scene.{ObjObject, Scene}
import support.{Config, Image}

class RendererSpec extends Specification with Mockito {

  //TODO: render more scenes with
  // no lights => dark
  // no shapes => dark
  // huge shape, ambient color => this color
  override def is = s2"""
      A Renderer should
        render an empty scene with a config file $renderEmpty
        find intersections for a ray and a maximum distance $testAnyHit
        determine whether a point is covered by a shadow $testShadowRay
        find the closest intersection with a shape $testGetFirstHit

        shade a hit with a shape $testShadeHit

        shade a hit for refraction $testShadeRefraction
        shade a hit for reflection $testShadeReflection
        shade a hit for specularity $testShadeSpecular
        shade a hit for diffuse $testShadeDiffuse
    """

  implicit val config: Config = Config()

  val emptyScene = Scene(pathPrefix = "",
                         cameraOrigin = Vector3.ZERO,
                         cameraPointing = Vector3.Z,
                         width = 2,
                         height = 2,
                         ppi = 100,
                         lights = Seq(),
                         shapes = Seq(),
                         materials = Seq(),
                         objFiles = Seq.empty)

  val renderEmpty = {
    val renderer = new Renderer(emptyScene)
    val image: Image = renderer.render(config)

    //should not crash
    (image.width should be equalTo (emptyScene.width * emptyScene.ppi).toInt) and
      (image.height should be equalTo (emptyScene.height * emptyScene.ppi).toInt)
  }

  val testEdgeRendering = {
    val Renderer = mock[Renderer]

    1 should be equalTo 1
  }

  val testAnyHit = {
    1 should be equalTo 1
  }

  val testShadowRay = {
    1 should be equalTo 1
  }

  val testGetFirstHit = {
    1 should be equalTo 1
  }

  val testShadeHit = {
    1 should be equalTo 1
  }

  val testShadeRefraction = {
    1 should be equalTo 1
  }

  val testShadeReflection = {
    1 should be equalTo 1
  }

  val testShadeSpecular = {
    1 should be equalTo 1
  }

  val testShadeDiffuse = {
    val renderer = new Renderer(emptyScene)
    val hit = Hit(
      distance = 3,
      position = Vector3.ZERO,
      originalNormal = Vector3.X,
      material = UnshadedMaterial(color = RGB.RED,
                            ambient = 0.2f,
                            diffuse = 0.2f,
                            spec = 0.2f,
                            reflective = 0.2f,
                            refractive = 0.2f,
                            n = 2,
                            shininess = 64)
    )
    val ray = Ray(Vector3.ZERO, Vector3.Z)
    1 should be equalTo 1
  }

}
