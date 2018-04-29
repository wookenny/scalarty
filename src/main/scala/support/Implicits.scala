package support

import java.awt.image.RenderedImage
import java.io.{File, FileInputStream}
import java.util.zip.GZIPInputStream
import javax.imageio.{IIOImage, ImageIO}

import scala.io.BufferedSource
import scala.io.Source.{fromFile, fromInputStream}

object Implicits {
  implicit val fileReader: String => BufferedSource = filename =>
    if (filename.endsWith(".gz"))
      fromInputStream(new GZIPInputStream(new FileInputStream(filename)))
    else fromFile(filename)

  implicit val imageWriter: ImageWriter = new ImageWriter {
    def write(image: RenderedImage, fileFormat: String, file: File) =
      ImageIO.write(image, fileFormat, file)
    val formats = ImageIO.getWriterFormatNames
  }

}
