package material

import java.awt.image.BufferedImage
import java.io.File

import color.RGB
import math.Vector3
import javax.imageio.ImageIO

import scala.util.Try

final case class WaveFrontMaterial(name: String,
                                   Ka: RGB,
                                   Kd: RGB,
                                   Ks: RGB,
                                   Tr: Double,
                                   Ns: Double,
                                   map_Kd: Option[BufferedImage]
                                  ) extends Material {

  val width  = map_Kd.map(_.getWidth).getOrElse(0)
  val height = map_Kd.map(_.getHeight).getOrElse(0)


  override def getMat(position: Vector3, uv_coordinates: Option[(Double, Double)]): UnshadedMaterial = {

    val diffuseColor = (map_Kd, uv_coordinates) match {
      case (Some(kdMap),Some((u,v))) => WaveFrontMaterial.sampleImage(u, v, kdMap, width, height)
      case _  => Kd
    }
    //TODO: set values
    //TODO: modify to allow colors for diffuse, specular, and so on
    UnshadedMaterial(
      color = diffuseColor,
      ambient = 0, //TODO: set based on illum 0 or 1
      diffuse = (Kd.red + Kd.green + Kd.blue)/3,
      spec = (Ks.red + Ks.green + Ks.blue)/3,
      reflective =  0,
      refractive = 0,
      n = 0,
      shininess = Ns,
      normalModifier = Vector3.ZERO,
    )
  }

}

object WaveFrontMaterial{

  def sampleImage(u: Double, v: Double, texture: BufferedImage, width : Int, height : Int) : RGB  = {
    val color = texture.getRGB( Math.round(u * width).toInt,
                                Math.round(v * height).toInt)
    //TODO: add linear or bilinear filtering
    RGB(color)
  }


  def fromFile(lines: String) : Seq[WaveFrontMaterial] = {
    val materials: Array[String] = lines.substring(lines.indexOf("newmtl"))
      .split("newmtl").filter(!_.isEmpty)
    for{material <- materials
        (name,lines) =  material.splitAt(material.indexOf("\n"))
        parameter  = lines.split("\n").filter(!_.isEmpty).map{ line =>
          val array = line.split("\\s+")
          array.head -> array.tail
        }.toMap

    } yield createMaterial(name.trim, parameter)

  }

  def createMaterial(name: String, paramter: Map[String,Array[String]]) : WaveFrontMaterial = {

    WaveFrontMaterial(name = name,
                      Ka = toRgb(paramter.getOrElse("Ka",Array.empty)),
                      Kd = toRgb(paramter.getOrElse("Ka",Array.empty)),
                      Ks = toRgb(paramter.getOrElse("Ka",Array.empty)),
                      Tr = if(paramter.contains("Tr"))
                              toDouble(paramter.getOrElse("Tr",Array.empty))
                            else if(paramter.contains("d"))
                              1 - toDouble(paramter.getOrElse("d",Array.empty))
                            else 0,
                      Ns = toDouble(paramter.getOrElse("Ns",Array.empty)),
                      map_Kd = paramter.get("map_Kd").flatMap(readImageFile))
  }

  //TODO: log errors
  def toRgb(array: Array[String]) = array match {
    case Array(x,y,z) => RGB(x.toDouble, y.toDouble, z.toDouble)
    case _            => RGB.BLACK
  }

  def toDouble(array: Array[String]) : Double = array match {
    case Array(x) => x.toDouble
    case _ => 0d
  }

  def readImageFile(array : Array[String]) : Option[BufferedImage] = array.headOption.flatMap{ filename =>
    Try(ImageIO.read(new File(filename))).toOption
  }

}
