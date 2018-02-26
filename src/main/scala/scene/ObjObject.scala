package scene

import com.typesafe.scalalogging.LazyLogging
import material.Material.DEFAULT_MATERIAL
import math.{Triangle, Vector3}
import math.Math.π

import scala.io.BufferedSource

case class ObjObject(filename: String,
                     centerBottom: Vector3,
                     maxSide: Double,
                     rotation: Double,
                     material: Option[String] = None)
    extends LazyLogging {

  val triangles = scala.collection.mutable.ArrayBuffer
    .empty[(Array[Int], Array[Int], Array[Int])]
  val normals = scala.collection.mutable.ArrayBuffer.empty[Vector3]
  val vertices = scala.collection.mutable.ArrayBuffer.empty[Vector3]

  private def transformVertex(vector: Vector3,
                              currentCenter: Vector3,
                              targetCenter: Vector3,
                              scalingFactor: Double,
                              rotation: Double): Vector3 = {

    val sin: Double = Math.sin(2 * π * rotation / 360)
    val cos: Double = Math.cos(2 * π * rotation / 360)
    val p = (vector - currentCenter) * scalingFactor
    Vector3(p.x * cos - p.z * sin, p.y, p.x * sin + p.z * cos) + targetCenter
  }

  private def transformNormal(vector: Vector3, rotation: Double) = {
    val sin: Double = Math.sin(2 * π * rotation / 360)
    val cos: Double = Math.cos(2 * π * rotation / 360)
    Vector3(vector.x * cos - vector.z * sin, vector.y, vector.x * sin + vector.z * cos)
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
      (vertices.map(_.x), vertices.map(_.y), vertices.map(_.z))
    val scalingFactor = maxSide / Seq(coordinates_x.max - coordinates_x.min,
                                      coordinates_y.max - coordinates_y.min,
                                      coordinates_z.max - coordinates_z.min).max

    val currentCenter = Vector3(
      (coordinates_x.max - coordinates_x.min) / 2 + coordinates_x.min,
      (coordinates_y.max - coordinates_y.min) / 2 + coordinates_y.min,
      (coordinates_z.max - coordinates_z.min) / 2 + coordinates_z.min
    )

    val height = (coordinates_y.max - coordinates_y.min)*scalingFactor
    val newCenter = centerBottom.copy(y = centerBottom.y + height/2)

    for (i <- vertices.indices.par)
      vertices(i) = transformVertex(vertices(i), currentCenter, newCenter, scalingFactor, rotation)

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
        vertices += Vector3(a.toDouble, b.toDouble, c.toDouble)
    }

  private def parseNormal(line: String) =
    line.split("\\s+").slice(1, 4) match {
      case Array(a, b, c) =>
        normals += Vector3(a.toDouble, b.toDouble, c.toDouble)
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
