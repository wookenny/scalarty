package support

import scala.io.BufferedSource
import scala.io.Source.fromFile

object Implicits {
  implicit val reader: String  => BufferedSource = fromFile
}
