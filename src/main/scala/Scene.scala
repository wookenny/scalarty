import java.awt.Color

import Material.SingleColorMaterial
import geometry.Light
import geometry.geometry.{AABB, Shape, Sphere, Vector3}

/**
  * Created by torsten on 9/18/16.
  */
class Scene {

  //TODO read a file as input
  val (width,height) = (2,2)
  val ppi =   400
  val cameraOrigin = Vector3(0,0,0)
  val cameraPointing = Vector3(0,0,1)
  val up = Vector3(0,1,0)
  val side = Vector3(1,0,0)

  val sphere1 =  Sphere(Vector3(0,0.8f,3),.1f)
    sphere1.material = SingleColorMaterial(Color.BLUE,0.1f,0.8f,.1f)
  val sphere2 = Sphere(Vector3(-.8f,-.5f,3),1f)
    sphere2.material=SingleColorMaterial(Color.CYAN,0.1f,0.8f,.1f)
  val sphere3 = Sphere(Vector3(0,-25f,0),23f)
    sphere3.material=SingleColorMaterial(Color.lightGray,0.05f,0.8f,.15f)



  val shapes : Seq[Shape] = Seq( Sphere(Vector3(0,0,2),.5f), sphere1, sphere2, sphere3,
                                 AABB(1, 2, -.8f, .2f, 2.1f, 3.1f) )

  val lights = Seq( Light(Vector3(4,2,0),Color.WHITE,.1f),
                    Light(Vector3(-4,2,0),Color.WHITE,1f))

}
