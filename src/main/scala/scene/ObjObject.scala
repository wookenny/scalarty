package scene

import com.typesafe.scalalogging.LazyLogging
import math.{Triangle, Vector3}
import play.api.libs.json.{Format, Json}

import scala.io.Source._


case class ObjObject(filename: String, center: Vector3, maxSide: Float, rotation: Float) extends LazyLogging{
  //TODO: texture name and smooth shading: boolean => vertices have an own normal

  var triangles = scala.collection.mutable.ListBuffer.empty[(Int,Int,Int)]
  var vertices  = scala.collection.mutable.ListBuffer.empty[Vector3]

  def transformVertex(vector: Vector3, currentCenter: Vector3, targetCenter: Vector3, scalingFactor: Float, rotation: Float): Vector3 = {


    val p = (vector - currentCenter) * scalingFactor

    //TODO: move rotation code to vector class and generalize
    val sin = Math.sin(rotation)
    val cos = Math.cos(rotation)
    Vector3(p.x * cos - p.z * sin,
            p.y,
            p.x * sin + p.z * cos) + targetCenter

  }

  def getTriangles : Seq[Triangle] = {
    //TODO: run only once
    val objFile = fromFile(filename).getLines

    objFile foreach{
      case line if line.trim.isEmpty => Unit //skip empty lines
      case line if line.trim.startsWith("#")  => Unit //comment
      case line if line.trim.startsWith("g") => Unit //TODO: parse normal
      case line if line.trim.startsWith("vn") => Unit //TODO: parse normal
      case line if line.trim.startsWith("v")  => parseVertex(line)
      case line if line.trim.startsWith("f")  => parseFace(line)
      case line => logger.error(s"ERROR: Cannot parse this line: <$line>")
    }

    val (coordinates_x, coordinates_y, coordinates_z) = (vertices.map(_.x), vertices.map(_.y), vertices.map(_.z))
    val scalingFactor = maxSide / Seq(coordinates_x.max - coordinates_x.min,
                                      coordinates_y.max - coordinates_y.min,
                                      coordinates_z.max - coordinates_z.min).max

    val currentCenter = Vector3((coordinates_x.max - coordinates_x.min)/2 + coordinates_x.min,
                         (coordinates_y.max - coordinates_y.min)/2 + coordinates_y.min,
                         (coordinates_z.max - coordinates_z.min)/2 + coordinates_z.min)

    for(i <- vertices.indices){
      vertices(i) = transformVertex(vertices(i), currentCenter, center, scalingFactor, rotation)
    }

    triangles.map{
      case (a,b,c) => Triangle(vertices(a-1), vertices(b-1), vertices(c-1))
    }
  }

  def parseVertex(line:  String) = {
    line.split("\\s+").slice(1,4) match {
      case Array(a, b, c) => vertices += Vector3(a.toFloat, b.toFloat, c.toFloat)
    }

  }

  def parseFace(line: String) = {
    line.split("\\s+").slice(1, 5) match {
      case Array(a, b, c, d) =>
        triangles += Tuple3(a.toInt, b.toInt, c.toInt)
        triangles += Tuple3(a.toInt, c.toInt, d.toInt)
      case Array(a, b, c) =>
        triangles += Tuple3(a.toInt, b.toInt, c.toInt)
    }
  }

}

object ObjObject {
  implicit val objObjectJsonFormat: Format[ObjObject] = Json.format[ObjObject]
}
