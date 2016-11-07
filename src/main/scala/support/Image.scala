package support

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

trait ImageWriterFactory {
  def create(w: Int, h: Int): ImageWriter
}

trait ImageWriter {
  def setRGB(x: Int, y: Int, c: Int): Unit
  def save(filename: String)
}

class Image(val width: Int, val height: Int) {
  require(width > 0, "Image width has to be positive.")
  require(height > 0, "Image height has to be positive.")

  private val alpha = 2.2
  private val img =
    new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  clear

  def save(filename: String): Boolean = {
    val (fixedfilename: String, fileType: String) = getFilenameEnding(filename)
    ImageIO.write(img, fileType, new File(fixedfilename))
  }

  private def getFilenameEnding(filename: String) =
    if (filename contains ".") {
      filename.splitAt(filename.lastIndexOf("."))
    } else
      (filename, ".png")

  def clear: Unit = {
    for {
      x <- 0 until width
      y <- 0 until height
    } {
      img.setRGB(x, y, Color.BLACK.getRGB)
    }
  }

  def set(x: Int, y: Int, c: Color): Unit = img.setRGB(x, y, c.getRGB)

}
