package support

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

import color.RGB

import scala.collection.GenSet
import scala.collection.parallel.immutable.{ParSeq, ParSet}

trait ImageWriterFactory {
  def create(w: Int, h: Int): ImageWriter
}

trait ImageWriter {
  def setRGB(x: Int, y: Int, c: Int): Unit
  def save(filename: String)
}

object Image {
  private val sobelKernelX = Seq(Seq(-1, 0, 1), Seq(-2, 0, 2), Seq(-1, 0, 1))
  private val sobelKernelY = Seq(Seq(-1, -2, -1), Seq(0, 0, 0), Seq(1, 2, 1))
}

class Image(val width: Int, val height: Int) {

  require(width > 0, "Image width has to be positive.")
  require(height > 0, "Image height has to be positive.")

  private val alpha = 2.2
  val edgeThreshold: Double = 1

  private val img =
    new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  clear

  def save(filename: String): Boolean = {
    val (fixedfilename: String, fileType: String) = getFilenameEnding(filename)
    ImageIO
      .write(img, fileType, new File(fixedfilename +"."+ fileType)) //TODO: use filetype!!!
  }

  private def getFilenameEnding(filename: String) =
    if (filename contains ".") {
      val index = filename.lastIndexOf(".")
      (filename.take(index), filename.drop(index+1).toLowerCase)
    } else
      (filename, "png")

  def clear: Unit = {
    for {
      x <- 0 until width
      y <- 0 until height
    } {
      img.setRGB(x, y, Color.BLACK.getRGB)
    }
  }

  def set(x: Int, y: Int, c: RGB): Boolean = (x, y) match {
    case (x, y) if x >= 0 && y >= 0 && x < img.getWidth && y < img.getHeight =>
      img.setRGB(x, y, c.awtColor().getRGB)
      true
    case _ => false
  }

  def get(x: Int, y: Int): Option[RGB] = (x, y) match {
    case (x, y) if x >= 0 && y >= 0 && x < img.getWidth && y < img.getHeight =>
      val color = new Color(img.getRGB(x, y))
      Some(
        RGB(color.getRed / 255f, color.getGreen / 255f, color.getBlue / 255f))
    case _ => None
  }

  def detectEdges(): GenSet[(Int, Int)] = {
    val pixelIter: Iterator[(Int, Int)] =
      for {
        x <- 0 until width iterator;
        y <- 0 until height iterator
      } yield (x, y)

    (pixelIter.toStream.par filter {
      case (x: Int, y: Int) => sobelFilterMagnitude(x, y) > edgeThreshold
    }).toSet
  }

  private def sobelFilterMagnitude(x: Int, y: Int): Double =
    Seq(sobelFilterMagnitute(x, y, (c: RGB) => c.red),
        sobelFilterMagnitute(x, y, (c: RGB) => c.green),
        sobelFilterMagnitute(x, y, (c: RGB) => c.blue)).max

  private def sobelFilterMagnitute(x: Int, y: Int, f: RGB => Double): Double =
    (for {
      a <- 0 to 2
      b <- 0 to 2
      xn = fitToX(x + a - 1)
      yn = fitToY(y + b - 1)
      rgb = get(xn, yn) if rgb.isDefined
      mx = f(rgb.get) * Image.sobelKernelX(a)(b)
      my = f(rgb.get) * Image.sobelKernelY(a)(b)
    } yield (mx, my)).foldLeft((0.0, 0.0)) {
      case ((a: Double, b: Double), (x: Double, y: Double)) => (a + x, b + y)
    } match {
      case (xi, yi) => Math.sqrt(xi * xi + yi * yi)
    }

  private def fitToX(x: Int) = 0 max (width - 1)  min  x
  private def fitToY(y: Int) = 0 max (height - 1) min  y

}
