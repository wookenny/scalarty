package support

import java.awt.image.RenderedImage
import java.io.File
import javax.imageio.ImageIO

import scala.io.BufferedSource
import scala.io.Source.fromFile

object Implicits {
  implicit val fileReader: String => BufferedSource = fromFile
  implicit val imageWriter: (RenderedImage, String, File) => Boolean =
    ImageIO.write
}
