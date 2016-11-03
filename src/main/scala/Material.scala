package Material

import geometry.{RGB, Vector3}
import play.api.libs.json.{Format, JsValue, Json}

object Material {
  val EPS = 0.001

  val DEFAULT_MATERIAL = SingleColorMaterial("DEFAULT_MATERIAL", RGB(.4,.4,.4),0.05f,0.75f,.15f,.05f)

  def unapply(material: Material): Option[(String, JsValue)] = {
    val (prod: Product, sub) = material match {
      case m: SingleColorMaterial => (m, Json.toJson(m)(singleColorMaterialFmt))
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: JsValue): Material = {
    (`type` match {
      case "SingleColorMaterial" => Json.fromJson[SingleColorMaterial](data)(singleColorMaterialFmt)
      case "CheckerMaterial" => Json.fromJson[CheckerMaterial](data)(checkerMaterialFmt)
    }).get
  }

  implicit val materialFmt : Format[Material] = Json.format[Material]
  implicit val singleColorMaterialFmt : Format[SingleColorMaterial] = Json.format[SingleColorMaterial]
  implicit val checkerMaterialFmt : Format[CheckerMaterial] = Json.format[CheckerMaterial]
}

final case class UnshadedColor(color: RGB, ambient: Float, diffuse: Float, spec: Float, reflective: Float, refractive: Float, n: Float, shininess: Float)

trait Material{
  def getMat(position: Vector3) : UnshadedColor
  def name: String
}


final case class SingleColorMaterial(name: String, c: RGB, ambient: Float, diffuse: Float, spec: Float, reflective : Float = 0.05f,  refractive: Float = 0, n: Float = 1.33f, shininess: Float = 64) extends Material {
  require( Math.abs(ambient+diffuse+spec+reflective+refractive - 1) <= Material.EPS )

  override def getMat(position: Vector3) = UnshadedColor(c, ambient, diffuse, spec, reflective, refractive, n, shininess)
}

final case class CheckerMaterial(name: String, c1: RGB, c2: RGB, steps: Float, ambient: Float, diffuse: Float, spec: Float, reflective : Float = 0.05f,  refractive: Float = 0, n: Float = 1.33f, shininess: Float = 64) extends Material {
  require( Math.abs(ambient+diffuse+spec+reflective+refractive - 1) <= Material.EPS )

  private def mod(x : Float, m: Float) = (x%m + m)%m

  private def inStep(pos: Float) : Boolean = mod(pos,2*steps) >= steps
  private def inStep(pos: Vector3) : Boolean = inStep(pos.x) ^ inStep(pos.y) ^ inStep(pos.z)
  override def getMat(position: Vector3) = UnshadedColor( if(inStep(position)) c1 else c2, ambient, diffuse, spec, reflective, refractive, n, shininess)
}