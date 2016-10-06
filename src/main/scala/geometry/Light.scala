package geometry

import java.awt.Color

import play.api.libs.json.{JsPath, Json, Reads}
import play.api.libs.json._
import play.api.libs.functional.syntax._


case class Light(position: Vector3, color: Color, intensity: Float)

