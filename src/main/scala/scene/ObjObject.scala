package scene

import com.typesafe.scalalogging.LazyLogging
import material.Material.DEFAULT_MATERIAL
import math.Triangle
import play.api.libs.json.{Format, Json}
import math.Math.π
import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._

import scala.io.BufferedSource

case class ObjObject(filename: String,
                     center: VectorBreeze3,
                     maxSide: Double,
                     rotation: Double,
                     material: Option[String] = None)
    extends LazyLogging {

  val triangles = scala.collection.mutable.ArrayBuffer
    .empty[(Array[Int], Array[Int], Array[Int])]
  val normals = scala.collection.mutable.ArrayBuffer.empty[VectorBreeze3]
  val vertices = scala.collection.mutable.ArrayBuffer.empty[VectorBreeze3]

  private def transformVertex(vector: VectorBreeze3,
                              currentCenter: VectorBreeze3,
                              targetCenter: VectorBreeze3,
                              scalingFactor: Double,
                              rotation: Double): VectorBreeze3 = {

    val sin: Double = Math.sin(2 * π * rotation / 360)
    val cos: Double = Math.cos(2 * π * rotation / 360)
    val p: VectorBreeze3 = (vector - currentCenter) * scalingFactor
    VectorBreeze3(p(0) * cos - p(2) * sin, p(1), p(0) * sin + p(2) * cos) + targetCenter
  }

  private def transformNormal(vector: VectorBreeze3, rotation: Double) = {
    val sin: Double = Math.sin(2 * π * rotation / 360)
    val cos: Double = Math.cos(2 * π * rotation / 360)
    VectorBreeze3(vector(0) * cos - vector(2) * sin, vector(1), vector(0) * sin + vector(2) * cos)
  }

  def getTriangles()(implicit reader: (String) => BufferedSource): Seq[Triangle] = {

    logger.info(s"Reading $filename")
    val objFile = reader(filename).getLines

    objFile foreach {
      case line if line.trim.isEmpty => Unit //skip empty lines
      case line if line.trim.startsWith("#") => Unit //comment
      case line if line.trim.startsWith("g") => Unit //TODO: parse what?
      case line if line.trim.startsWith("vn") => parseNormal(line)
      case line if line.trim.startsWith("v") => parseVertex(line)
      case line if line.trim.startsWith("f") => parseFace(line)
      case line => logger.error(s"ERROR: Cannot parse this line: <$line>")
    }

    val (coordinates_x, coordinates_y, coordinates_z) =
      (vertices.map(v => v(0)), vertices.map(v => v(1)), vertices.map(v => v(2)))
    val scalingFactor = maxSide / Seq(coordinates_x.max - coordinates_x.min,
                                      coordinates_y.max - coordinates_y.min,
                                      coordinates_z.max - coordinates_z.min).max

    val currentCenter = VectorBreeze3(
      (coordinates_x.max - coordinates_x.min) / 2 + coordinates_x.min,
      (coordinates_y.max - coordinates_y.min) / 2 + coordinates_y.min,
      (coordinates_z.max - coordinates_z.min) / 2 + coordinates_z.min
    )

    for (i <- vertices.indices.par)
      vertices(i) = transformVertex(vertices(i), currentCenter, center, scalingFactor, rotation)

    for (i <- normals.indices.par)
      normals(i) = transformNormal(normals(i), rotation).normalized

    val mat: String = material.getOrElse(DEFAULT_MATERIAL.name)

    val ts =
      if (normals.size >= vertices.size)
        triangles.par.map {
          case (a, b, c) =>
            val norms = Some(Seq(normals(a.last - 1), normals(b.last - 1), normals(c.last - 1)))
            Triangle(vertices(a.head - 1), vertices(b.head - 1), vertices(c.head - 1), mat, norms)
        } else
        triangles.par.map {
          case (a, b, c) =>
            Triangle(vertices(a.head - 1), vertices(b.head - 1), vertices(c.head - 1), mat)
        }

    logger.info(
      s"Object read with ${vertices.size} vertices, ${normals.size} normals and ${triangles.size} triangles")
    ts.toList
  }

  private def parseVertex(line: String) =
    line.split("\\s+").slice(1, 4) match {
      case Array(a, b, c) =>
        vertices += VectorBreeze3(a.toDouble, b.toDouble, c.toDouble)
    }

  private def parseNormal(line: String) =
    line.split("\\s+").slice(1, 4) match {
      case Array(a, b, c) =>
        normals += VectorBreeze3(a.toDouble, b.toDouble, c.toDouble)
    }

  private def toIntList(s: String): Array[Int] =
    s.trim.split("/").filter(_ != "").map(_.trim.toInt)

  private def parseFace(line: String) = {
    line.split("\\s+").slice(1, 5) match {
      case Array(a, b, c, d) =>
        triangles += Tuple3(toIntList(a), toIntList(b), toIntList(c))
        triangles += Tuple3(toIntList(a), toIntList(c), toIntList(d))
      case Array(a, b, c) =>
        triangles += Tuple3(toIntList(a), toIntList(b), toIntList(c))
    }
  }

}

object ObjObject {
  implicit val objObjectJsonFormat: Format[ObjObject] = Json.format[ObjObject]
}
