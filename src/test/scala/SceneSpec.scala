import color.RGB
import lightning.Light
import material.{CheckerMaterial, Material, SingleColorMaterial}
import math.{Shape, Triangle, Vector3}
import org.specs2.Specification
import org.specs2.mock.Mockito
import org.specs2.specification.core.SpecStructure
import scene.{ObjObject, Scene}
import support.Implicits._


class SceneSpec extends Specification with Mockito{

  override def is: SpecStructure = s2"""
    A scene
      without shapes should be initialized correctly $testSceneWithoutObjInit
      with shapes and obj files should be initialized correctly $testSceneWithObjInit
    """

  val testSceneWithoutObjInit = {
    val mat1 = SingleColorMaterial("mat1", RGB.CYAN, 0.7f, 0.1f, 0.1f, 0.1f)
    val mat2 = CheckerMaterial("mat2", RGB.GREEN, RGB(0.4f, 0.5f, 0.1f),
                                0.2, 1, 0, 0)

    val (shape1,shape2) = (mock[Shape],mock[Shape])
    val lights = Seq(Light(Vector3(1, 2, 3), RGB.WHITE, 12))

    val scene = Scene(Vector3.ZERO, Vector3.Z, 2, 2,
                      lights,
                      Seq(shape1,shape2),
                      Seq(mat1, mat2))

    (Shape.materialMap should havePairs("mat1" -> mat1, "mat2" -> mat2)) and
      (scene.materials should be equalTo Seq(mat1, mat2)) and
      (scene.up should be equalTo Vector3.Y) and
      (scene.side should be equalTo Vector3.X) and
      (scene.objFiles should be equalTo None) and
      (scene.allShapes.size should be equalTo 2) and
      (scene.cameraOrigin should be equalTo Vector3.ZERO)
      (scene.cameraPointing should be equalTo Vector3.Z) and
      (scene.lights should be equalTo lights) and
      ((scene.height,scene.width) should be equalTo (2,2))
  }

  val testSceneWithObjInit = {
    val mat1 = Material.DEFAULT_MATERIAL.copy(name="def_mat")

    val (shape1,shape2) = (mock[Shape],mock[Shape])
    val lights = Seq(Light(Vector3(3, 2, 5), RGB.CYAN, 2))

    val (objObj1,objObj2) = (mock[ObjObject],mock[ObjObject])
    val (triangle1,triangle2,triangle3) = (Triangle(Vector3.X,Vector3.Y,Vector3.Z),
                                            Triangle(Vector3.Y,Vector3.X,Vector3.Z),
                                            Triangle(Vector3.Z,Vector3.Y,Vector3.X))
    objObj1.getTriangles returns Seq(triangle1)
    objObj2.getTriangles returns Seq(triangle2,triangle3)

    val scene = Scene(Vector3.ONE, Vector3.X, 4, 7,
      lights,
      Seq(shape1,shape2),
      Seq(mat1),
      Some(Seq(objObj1,objObj2)))

    //println(s"some tests: ${scene.allShapes.size}")
    (Shape.materialMap should havePair("def_mat" -> mat1)) and
      (scene.materials should be equalTo Seq(mat1)) and
      (scene.objFiles.getOrElse(Seq.empty) should contain(exactly(objObj1,objObj2))) and
      (scene.allShapes.size should be equalTo 5) and
      (scene.lights should be equalTo lights) and
      ((scene.width ,scene.height) should be equalTo (4,7))
  }

  //TODO: BVH, ShapeContainer,... should be injected for nicer tests

}
