package scene

import lightning.Light
import material.Material
import math.{Shape, Vector3}
import play.api.libs.json.{Format, JsValue, Json}

final case class Scene(cameraOrigin : Vector3, cameraPointing: Vector3, width: Float, height: Float,
                 lights: Seq[Light], shapes: Seq[Shape], materials: Seq[Material]) {

  // Fixed data
  val ppi =   400
  val up   = Vector3(0,1,0)
  val side = Vector3(1,0,0)
  Shape.materialMap = materials.groupBy(_.name).mapValues(_.head)

}

object Scene {
  implicit val sceneJsonFormat : Format[Scene] = Json.format[Scene]
}

