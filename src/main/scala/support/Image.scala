package support

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

import color.RGB

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
  val edgeThreshold : Double = 1

  private val img =  new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  clear

  def save(filename: String): Boolean = {
    val (fixedfilename: String, fileType: String) = getFilenameEnding(filename)
    ImageIO.write(img, "png", new File(fixedfilename+fileType)) //TODO: use filetype!!!
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

  //todo: what about wrong range?
  def set(x: Int, y: Int, c: RGB): Unit = img.setRGB(x, y, c.awtColor().getRGB)

  //TODO: what about wrong range?
  def get(x: Int, y: Int): RGB = {
    val color = new Color(img.getRGB(x, y))
    RGB(color.getRed/255f, color.getGreen/255f, color.getBlue/255f)
  }

  def detectEdges() : Set[(Int,Int)] =
      (for {x <- 0 until width
            y <- 0 until height if sobelFilterMagnitude(x, y) > edgeThreshold}
      yield (x, y) ).toSet

  private def sobelFilterMagnitude(x: Int, y: Int) : Double = {
    val kernelx = Seq(Seq(-1,0,1),
                      Seq(-2,0,2),
                      Seq(-1,0,1))

    val kernely = Seq(Seq(-1,-2,-1),
                      Seq( 0, 0, 0),
                      Seq( 1, 2, 1))

    Seq(sobelFilterMagnitute(x, y, kernelx, kernely, (c: RGB) => c.red),
        sobelFilterMagnitute(x, y, kernelx, kernely, (c: RGB) => c.green),
        sobelFilterMagnitute(x, y, kernelx, kernely, (c: RGB) => c.blue)).max

  }

  def sobelFilterMagnitute(x: Int, y: Int, kernelx: Seq[Seq[Int]], kernely: Seq[Seq[Int]], f: RGB => Float): Double = {

    (for {
      a <- 0 to 2
      b <- 0 to 2
      xn = fitToX(x + a - 1)
      yn = fitToY(y + b - 1)
      rgb = get(xn, yn)
      mx = f(rgb) * kernelx(a)(b)
      my = f(rgb) * kernely(a)(b)
    } yield (mx, my)
      ).foldLeft((0f, 0f)) { case ((a: Float, b: Float), (x: Float, y: Float)) => (a + x, b + y) }
    match{
      case (x,y) => Math.sqrt(x*x+y*y)
    }
  }

  private def fitToX(x:Int) = Math.max(0,Math.min(width-1,x))
  private def fitToY(y:Int) = Math.max(0,Math.min(height-1,y))

}
