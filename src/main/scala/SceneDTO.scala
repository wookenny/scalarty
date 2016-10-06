import geometry.Vector3
import play.api.libs.json.Json

case class SceneDTO(cameraOrigin : String, cameraPointing : String, width: Float, height : Float, lights: Seq[String] ){

  println(s"lights: $lights")
  lazy val getCameraOrigin  = Vector3.fromString(cameraOrigin)
  lazy val getCameraPointing  = Vector3.fromString(cameraPointing)


}

object SceneDTO{
  implicit val sceneJsonFormat = Json.reads[SceneDTO]
}