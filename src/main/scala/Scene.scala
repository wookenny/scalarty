import Material.SingleColorMaterial
import geometry._
import play.api.libs.json.Json
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsValue, Json, _}

case class Scene(cameraOrigin : Vector3, cameraPointing: Vector3, width: Float, height: Float,
                 lights: Seq[Light]) {

  //TODO: move into file:
  val sphere1 =  Sphere(Vector3(0,0.8f,3),.1f)
  sphere1.material = SingleColorMaterial(RGB.BLUE,0.05f,0.75f,.15f,.05f)
  val sphere2      = Sphere(Vector3(-.8f,-.5f,3),1f)
  sphere2.material = SingleColorMaterial(RGB.CYAN,0.05f,0.75f,.15f,.05f)
  val sphere3      = Sphere(Vector3(1,0,2),.5f)
  sphere3.material = SingleColorMaterial(RGB(.5f,.5f,.5f),0.05f,0.65f,.15f,.15f)


  val a = Vector3(0, 0, 2)
  val b = Vector3(.2f,.2f,2)
  val c = Vector3(-.2f,.2f, 2)
  val shapes : Seq[Shape] = Seq( Sphere(Vector3(0,-25f,0),23f), sphere1, sphere2, sphere3,
   AABB(1, 2, -.8f, .2f, 2.1f, 3.1f)
    //Triangle(a,b,c))
  )

  // Fixed data
  val ppi =   400
  val up = Vector3(0,1,0)
  val side = Vector3(1,0,0)

}

object Scene {
  implicit val sceneJsonFormat = Json.format[Scene]
}

