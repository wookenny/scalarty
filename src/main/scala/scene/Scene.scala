package scene

import bounding.{BVH, ShapeContainer, ShapeMetaContainer, ShapeSeq}
import lightning.LightSource
import material.Material
import math.breeze.VectorBreeze3
import math.{Shape, Triangle}
import play.api.libs.json.{Format, Json}
import support.Config
import support.Implicits._
import math.breeze.VectorBreeze3._


final case class SceneDTO(cameraOrigin: VectorBreeze3,
                          cameraPointing: VectorBreeze3,
                          width: Double,
                          height: Double,
                          lights: Seq[LightSource],
                          shapes: Seq[Shape],
                          materials: Seq[Material],
                          objFiles: Option[Seq[ObjObject]] = None) {}

object SceneDTO {
  implicit val sceneJsonFormat: Format[SceneDTO] = Json.format[SceneDTO]
}

case class Scene(cameraOrigin: VectorBreeze3,
                       cameraPointing: VectorBreeze3,
                       width: Double,
                       height: Double,
                       lights: Seq[LightSource],
                       shapes: Seq[Shape],
                       materials: Seq[Material],
                       objFiles: Option[Seq[ObjObject]] = None)(implicit config: Config) {
  // Fixed data
  val ppi = 400
  val up : VectorBreeze3 = VectorBreeze3.Y
  val side : VectorBreeze3 = VectorBreeze3.X
  lazy val allShapes: ShapeContainer =
    if (objFiles.isDefined)
      ShapeMetaContainer(ShapeSeq(shapes), BVH(parseObjFiles(objFiles.get)))
    else
      ShapeSeq(shapes)
  Shape.materialMap = materials.groupBy(_.name).mapValues(_.head)

  private def parseObjFiles(objFiles: Seq[ObjObject]): Array[Triangle] = {
    objFiles.flatMap(_.getTriangles).toArray
  }

}

object Scene {
  def fromDTO(sceneDTO: SceneDTO)(implicit config: Config) =
    Scene(sceneDTO.cameraOrigin,
          sceneDTO.cameraPointing,
          sceneDTO.width,
          sceneDTO.height,
          sceneDTO.lights,
          sceneDTO.shapes,
          sceneDTO.materials,
          sceneDTO.objFiles)
}
