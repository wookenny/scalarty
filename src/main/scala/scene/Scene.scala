package scene

import bounding.{BVH, ShapeContainer, ShapeMetaContainer, ShapeSeq}
import lightning.LightSource
import material.Material
import math.{Shape, Triangle, Vector3}
import play.api.libs.json.{Format, Json}
import support.Config
import support.Implicits._

//TODO: use gzip

final case class SceneDTO(cameraOrigin: Vector3,
                          cameraPointing: Vector3,
                          width: Double,
                          height: Double,
                          lights: Seq[LightSource],
                          shapes: Seq[Shape],
                          materials: Seq[Material],
                          objFiles: Option[Seq[ObjObject]] = None) {}

object SceneDTO {
  implicit val sceneJsonFormat: Format[SceneDTO] = Json.format[SceneDTO]
}

case class Scene(cameraOrigin: Vector3,
                       cameraPointing: Vector3,
                       width: Double,
                       height: Double,
                       lights: Seq[LightSource],
                       shapes: Seq[Shape],
                       materials: Seq[Material],
                       objFiles: Option[Seq[ObjObject]] = None)(implicit config: Config) {
  // Fixed data
  val ppi = 400
  val up = Vector3(0, 1, 0)
  val side = Vector3(1, 0, 0)
  lazy val allShapes: ShapeContainer =
    if (objFiles.isDefined)
      ShapeMetaContainer(ShapeSeq(shapes), BVH(parseObjFiles(objFiles.get),config.bvhSplitLimit,config.sah))
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
