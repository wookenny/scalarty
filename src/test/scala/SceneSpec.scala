import color.RGB
import lightning.Light
import material.{CheckerMaterial, SingleColorMaterial}
import math.{Shape, Vector3}
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import scene.Scene

class SceneSpec extends Specification {

  override def is: SpecStructure = s2"""
    A scene should
      be initialized coirrectly $testSceneInit
    """

  val testSceneInit = {
    val mat1 = SingleColorMaterial("mat1", RGB.CYAN, 0.7f, 0.1f, 0.1f, 0.1f)
    val mat2 = CheckerMaterial("mat2",
                               RGB.GREEN,
                               RGB(0.4f, 0.5f, 0.1f),
                               .2f,
                               1,
                               0,
                               0,
                               0)
    val scene = Scene(Vector3.ZERO,
                      Vector3.Z,
                      2f,
                      2f,
                      Seq(Light(Vector3(1, 2, 3), RGB.WHITE, 12)),
                      Seq.empty,
                      Seq(mat1, mat2),
                      None)

    Shape.materialMap should havePairs("mat1" -> mat1, "mat2" -> mat2)
    scene.up should be equalTo Vector3.Y
    scene.side should be equalTo Vector3.X
  }

}
