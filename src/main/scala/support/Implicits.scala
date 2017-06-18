package support

import java.awt.image.RenderedImage
import java.io.{File, FileInputStream}
import java.util.zip.GZIPInputStream
import javax.imageio.ImageIO
import scala.io.Source.{fromFile,fromInputStream}
import scala.io.{BufferedSource, Source}

object Implicits {
  implicit val fileReader: String => BufferedSource = filename => if(filename.endsWith(".gz"))
                                                                       fromInputStream(new GZIPInputStream(new FileInputStream(filename)))
                                                                  else fromFile(filename)

  implicit val imageWriter: (RenderedImage, String, File) => Boolean =
    ImageIO.write
}
