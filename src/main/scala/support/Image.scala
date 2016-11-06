package support


import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import java.io.File
import javax.imageio.ImageIO


class Image(val width: Int, val height: Int){
  require(width>=0)
  require(height>=0)


  private val alpha = 2.2
  private val img = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)
  private val g2d: Graphics2D = img.createGraphics
  init

  def save(filename: String) : Boolean = {
    val (fixedfilename:String,fileType:String) = getFilenameEnding(filename)
    ImageIO.write(img, fileType,  new File(fixedfilename))
  }

  private def getFilenameEnding(filename: String) =
    filename.split(".").lastOption match {
      //TODO check that is is at ending, not in between
      case Some(ending) => (filename.drop(ending.size), ending)
      case None => (filename, "png")
    }

  def set(x: Int, y: Int, c: Color) : Unit = {
    img.setRGB(x,y,c.getRGB)
  }

  def init : Unit = {
    g2d setColor Color.BLACK
    g2d.fillRect(0,0,width,height)
  }


}
