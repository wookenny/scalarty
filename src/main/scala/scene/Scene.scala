package scene

import bounding.{BVH, ShapeContainer, ShapeMetaContainer, ShapeSeq}
import lightning.LightSource
import material.Material
import math.{Shape, Triangle, Vector3}
import play.api.libs.json.{Format, Json}
import support.Config
import support.Implicits._

final case class SceneDTO(cameraOrigin: Vector3,
                       cameraPointing: Vector3,
                       width: Double,
                       height: Double,
                       lights: Seq[LightSource],
                       shapes: Seq[Shape],
                       materials: Seq[Material],
                       objFiles: Option[Seq[ObjObject]] = None) {
}

object SceneDTO {
  implicit val sceneJsonFormat: Format[SceneDTO] = Json.format[SceneDTO]
}

final case class Scene(scene: SceneDTO)(implicit config: Config) {
  // Fixed data
  val ppi = 400
  val up = Vector3(0, 1, 0)
  val side = Vector3(1, 0, 0)
  lazy val allShapes: ShapeContainer =
    if (scene.objFiles.isDefined)
      ShapeMetaContainer(ShapeSeq(shapes), BVH(parseObjFiles(objFiles.get)))
    else
      ShapeSeq(shapes)
  Shape.materialMap = materials.groupBy(_.name).mapValues(_.head)

  private def parseObjFiles(objFiles: Seq[ObjObject]): Array[Triangle] = {
    objFiles.flatMap(_.getTriangles).toArray
  }

  def cameraOrigin = scene.cameraOrigin
  def cameraPointing =  scene.cameraPointing
  def width = scene.width
  def height = scene.height
  def lights = scene.lights
  def shapes = scene.shapes
  def materials = scene.materials
  def objFiles = scene.objFiles

}


