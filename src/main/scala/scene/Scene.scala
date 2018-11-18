package scene

import bounding.{BVH, ShapeContainer, ShapeMetaContainer, ShapeSeq}
import lightning.LightSource
import material.Material
import math.{Shape, Triangle, Vector3}
import support.Config
import support.Implicits._



case class SceneDTO(
    cameraOrigin: Vector3,
    cameraPointing: Vector3,
    width: Double,
    height: Double,
    ppi: Int,
    lights: Seq[LightSource],
    shapes: Seq[Shape],
    materials: Seq[Material],
    objFiles: Option[Seq[ObjObject]] = None
) {}

case class Scene(
    pathPrefix : String,
    cameraOrigin: Vector3,
    cameraPointing: Vector3,
    width: Double,
    height: Double,
    ppi: Int,
    lights: Seq[LightSource],
    shapes: Seq[Shape],
    materials: Seq[Material],
    objFiles: Seq[ObjObject] = Seq.empty
)(implicit config: Config) {
  // Fixed data
  val up = Vector3(0, 1, 0)
  val side = Vector3(1, 0, 0)

  //TODO: Big triangles/quads could/should be decomposed into smaller ones for speedup
  val allShapes: ShapeContainer =
    if (objFiles.nonEmpty)
      ShapeMetaContainer(
        ShapeSeq(shapes),
        BVH(parseObjFiles(objFiles.toVector, pathPrefix), config.bvhSplitLimit, config.sah)
      )
    else
      ShapeSeq(shapes)

  Shape.materialMap = materials.groupBy(_.name).mapValues(_.head)

  private def parseObjFiles(objFiles: Vector[ObjObject], pathPrefix: String): Vector[Triangle] = {
    objFiles.map(objFile => objFile.copy(pathPrefix = pathPrefix + objFile.pathPrefix)).flatMap(_.getTriangles)
  }

}

object Scene {

  val pathSeparator = "/" //TODO: File.pathSeparator was not working??

  def getFolderOfFile(filepath: String) = {
    filepath.substring(0,filepath.lastIndexOf(pathSeparator)+1)
  }

  def fromDTO(sceneDTO: SceneDTO, path: String = "")(implicit config: Config) =
    Scene(
      getFolderOfFile(path),
      sceneDTO.cameraOrigin,
      sceneDTO.cameraPointing,
      sceneDTO.width,
      sceneDTO.height,
      sceneDTO.ppi,
      sceneDTO.lights,
      sceneDTO.shapes,
      sceneDTO.materials,
      sceneDTO.objFiles.getOrElse(Seq.empty)
    )
}
