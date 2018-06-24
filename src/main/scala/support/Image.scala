package support

import java.awt.Color
import java.awt.image.{BufferedImage, RenderedImage}
import java.io.File

import color.RGB
import color.RGB.BLACK

import scala.collection.GenSet

trait ImageWriter {
  //TODO: Write Metadata as well (-> inputfile, render settings, render time)
  def write(image: RenderedImage, fileFormat: String, file: File): Boolean
  def formats: Seq[String]
}

object Image {
  private val sobelKernelX = Seq(Seq(-1, 0, 1), Seq(-2, 0, 2), Seq(-1, 0, 1))
  private val sobelKernelY = Seq(Seq(-1, -2, -1), Seq(0, 0, 0), Seq(1, 2, 1))
  private val DefaultFormat = "png"

  private def get(x: Int, y: Int, image: BufferedImage): Option[RGB] = (x, y) match {
    case (a, b) if a >= 0 && b >= 0 && a < image.getWidth && b < image.getHeight =>
      val color = new Color(image.getRGB(x, y))
      Some(RGB(color.getRed * 255f, color.getGreen * 255f, color.getBlue * 255f))
    case _ => None
  }
}

case class Pixel(color: RGB = BLACK, depth: Double = 0d, shadow: Double = 0d)

class Image(val width: Int, val height: Int) {
  import Image._
  require(width > 0, "Image width has to be positive.")
  require(height > 0, "Image height has to be positive.")

  private val alpha = 2.2
  val edgeThreshold: Double = 1

  private val image: Array[Array[Pixel]] = Array.fill(width) { Array.fill(height) { Pixel() } }

  def getImageLayer(f: Pixel => RGB): BufferedImage = {
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for {
      x <- 0 until width
      y <- 0 until height
    } {
      img.setRGB(x, y, f(image(x)(y)).awtColor.getRGB)
    }
    img
  }

  def colorImage = getImageLayer(_.color)
  def shadowImage = getImageLayer(RGB.WHITE * _.shadow)
  def depthImage = {
    if (image == null)
      println("\n\n\nNULL!!!!!\n\n\n")
    val depths = image.flatMap(_.map(_.depth).filter(_ != Double.PositiveInfinity))
    val maxDepth = depths.max
    val minDepth = depths.min
    getImageLayer(p => RGB.WHITE * (p.depth - minDepth) / (maxDepth - minDepth))
  }
  def save(filename: String)(implicit writer: ImageWriter): Boolean = {
    val (fixedfilename: String, fileType: String) = getFilenameEnding(filename)
    writer.write(colorImage, fileType, new File(s"$fixedfilename.${validatedFileFormat(fileType)}"))
  }

  private def getFilenameEnding(filename: String) =
    if (filename contains ".") {
      val index = filename.lastIndexOf(".")
      (filename.take(index), filename.drop(index + 1).toLowerCase)
    } else
      (filename, DefaultFormat)

  private def validatedFileFormat(format: String)(implicit writer: ImageWriter): String = {
    if (writer.formats contains format)
      format
    else DefaultFormat
  }

  def set(x: Int, y: Int, c: RGB, depth: Double = 0, shadow: Double = 0): Boolean = (x, y) match {
    case (a, b) if a >= 0 && b >= 0 && a < width && b < height =>
      image(x)(y) = Pixel(c, depth, shadow)
      true
    case _ => false
  }

  def get(x: Int, y: Int): Option[Pixel] = (x, y) match {
    case (a, b) if a >= 0 && b >= 0 && a < width && b < height =>
      Some(image(x)(y))
    case _ => None
  }

  def get(x: Int, y: Int, image: BufferedImage): Option[RGB] = (x, y) match {
    case (a, b) if a >= 0 && b >= 0 && a < image.getWidth && b < image.getHeight =>
      val color = new Color(image.getRGB(a, b))
      Some(RGB(color.getRed / 255f, color.getGreen / 255f, color.getBlue / 255f))
    case _ => None
  }

  def detectEdges(img: => BufferedImage = colorImage): GenSet[(Int, Int)] = {
    val imageToFilter = img
    val pixelIter: Iterator[(Int, Int)] =
      for {
        x <- 0 until width iterator;
        y <- 0 until height iterator
      } yield (x, y)

    (pixelIter.toStream.par filter {
      case (x: Int, y: Int) => sobelFilterMagnitude(x, y, imageToFilter) > edgeThreshold
    }).toSet
  }

  def detectShadowEdges() = detectEdges(shadowImage)

  private def sobelFilterMagnitude(x: Int, y: Int, imageToFilter: BufferedImage): Double =
    sobelFilterMagnitute(x, y, (c: RGB) => c.red, imageToFilter) max
      sobelFilterMagnitute(x, y, (c: RGB) => c.green, imageToFilter) max
      sobelFilterMagnitute(x, y, (c: RGB) => c.blue, imageToFilter)

  private def sobelFilterMagnitute(
      x: Int,
      y: Int,
      f: RGB => Double,
      imageToFilter: BufferedImage
  ): Double = {
    val kernelFiltered: Seq[(Double, Double)] = for {
      a <- 0 to 2
      b <- 0 to 2
      xn = clampX(x + a - 1)
      yn = clampY(y + b - 1)
      rgb = get(xn, yn, imageToFilter) if rgb.isDefined
      value: Double = rgb.map(f).getOrElse(0)
    } yield (value * sobelKernelX(a)(b), value * sobelKernelY(a)(b))

    kernelFiltered.foldLeft((0.0, 0.0)) {
      case ((a: Double, b: Double), (x: Double, y: Double)) =>
        (a + x, b + y) //first sum up
    } match {
      case (xi, yi) => Math.sqrt(xi * xi + yi * yi)
    }
  }

  private def clampX(x: Int) = 0 max (width - 1) min x
  private def clampY(y: Int) = 0 max (height - 1) min y

}
