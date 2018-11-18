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

      UnshadedMaterial.fromWaveFrontMaterial( Ka, diffuseColor, Ks, Tr, Ns)
  }

}

object WaveFrontMaterial{

  def sampleImage(u: Double, v: Double, texture: BufferedImage, width : Int, height : Int) : RGB  = {
    val color: Int = texture.getRGB( Math.round(u * width).toInt,
                                Math.round(v * height).toInt)
    //linear filtering
    val x =  Math.floor(u * width).toInt
    val X = (x+1) % width
    val y =  Math.floor(u * width).toInt
    val Y = (y+1) % width
    val Seq(f00,f10,f01,f11) = Seq((x,y),(X,y),(x,Y),(X,Y)).map(Function.tupled(texture.getRGB)).map(RGB.apply)


    val result = f00 + (f10 - f00) * (u-x)
                     + (f01 - f00) * (v-y)
                     + (f11 + f00 - f10 - f01) * (u-x) * (v-y)

    result
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
  def toRgb(array: Array[String]): RGB = array match {
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
