import java.awt.Color

import Material.SingleColorMaterial
import geometry._
import play.api.libs.json.Json

/**
  * Created by torsten on 9/18/16.
  */
case class Scene(val cameraOrigin     : Vector3 = Vector3.ZERO,
                 val cameraPointing   : Vector3 = Vector3.Z ) {

  //TODO: move into file:
  val (width,height) = (2,2)

  val sphere1 =  Sphere(Vector3(0,0.8f,3),.1f)
  sphere1.material = SingleColorMaterial(Color.BLUE,0.1f,0.8f,.1f)
  val sphere2 = Sphere(Vector3(-.8f,-.5f,3),1f)
  sphere2.material=SingleColorMaterial(Color.CYAN,0.1f,0.8f,.1f)
  val sphere3 = Sphere(Vector3(0,-25f,0),23f)
  sphere3.material=SingleColorMaterial(Color.lightGray,0.05f,0.8f,.15f)


  val a = Vector3(0, 0, 2)
  val b = Vector3(.2f,.2f,2)
  val c = Vector3(-.2f,.2f, 2)
  val shapes : Seq[Shape] = Seq( Sphere(Vector3(0,0,2),.5f), //sphere1, sphere2, sphere3,
    AABB(1, 2, -.8f, .2f, 2.1f, 3.1f),
    Triangle(a,b,c))

  val lights = Seq( Light(Vector3(4,2,0),Color.WHITE,.1f),
    Light(Vector3(-4,2,0),Color.WHITE,1f))

  // Fixed data
  val ppi =   400
  val up = Vector3(0,1,0)
  val side = Vector3(1,0,0)

}

object  Scene {
  implicit val sceneJsonFormat = Json.format[Scene]
}

