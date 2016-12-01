package scene


import lightning.Light
import material.Material
import math.{Shape, Triangle, Vector3}
import bounding.{BVH, ShapeContainer, ShapeMetaContainer, ShapeSeq}
import play.api.libs.json.{Format, Json}

final case class Scene(cameraOrigin: Vector3,
                       cameraPointing: Vector3,
                       width: Float,
                       height: Float,
                       lights: Seq[Light],
                       shapes: Seq[Shape],
                       materials: Seq[Material],
                       objFiles: Option[Seq[ObjObject]]) {

  // Fixed data
  val ppi = 400
  val up = Vector3(0, 1, 0)
  val side = Vector3(1, 0, 0)
  lazy val allShapes : ShapeContainer = if(objFiles.isDefined)
                          ShapeMetaContainer( ShapeSeq(shapes), BVH( parseObjFiles(objFiles.get)) )
                          ////TODO Multiple BVH's for each object? Or is it ok?
                        else
                          ShapeSeq(shapes)
  Shape.materialMap = materials.groupBy(_.name).mapValues(_.head)


  def parseObjFiles(objFiles: Seq[ObjObject]) : Seq[Triangle] = {
    objFiles.flatMap(_.getTriangles)
  }
}



object Scene {
  implicit val sceneJsonFormat: Format[Scene] = Json.format[Scene]
}
