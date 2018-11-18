package scene

import com.typesafe.scalalogging.LazyLogging
import material.Material.DEFAULT_MATERIAL
import math.{Triangle, Vector3}
import math.Math.π

import scala.io.BufferedSource
import scala.util.{Failure, Success, Try}

case class ObjObject(
    pathPrefix: String,
    filename: String,
    centerBottom: Vector3,
    maxSide: Double,
    rotation: Double,
    material: Option[String] = None
) extends LazyLogging {

  val triangles = scala.collection.mutable.ArrayBuffer.empty[(FaceVertex, FaceVertex, FaceVertex)]
  val normals = scala.collection.mutable.ArrayBuffer.empty[Vector3]
  val vertices = scala.collection.mutable.ArrayBuffer.empty[Vector3]
  val textureCoordinates = scala.collection.mutable.ArrayBuffer.empty[(Double,Double)]

  private def transformVertex(
      vector: Vector3,
      currentCenter: Vector3,
      targetCenter: Vector3,
      scalingFactor: Double,
      rotation: Double
  ): Vector3 = {

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

  def getTriangles()(implicit reader: String => BufferedSource): Seq[Triangle] = {

    logger.info(s"Reading $filename from folder $pathPrefix")

    val fileReadSuccessfully: Try[Unit] = Try(reader(pathPrefix+filename)).map(_.getLines() foreach {
        case line if line.trim.isEmpty          => Unit //skip empty lines
        case line if line.trim.startsWith("#")  => Unit //comment
        case line if line.trim.startsWith("g")  => Unit //TODO: parse what?
        case line if line.trim.startsWith("vn") => parseNormal(line)
        case line if line.trim.startsWith("vt") => parseTextureCoordinate(line)
        case line if line.trim.startsWith("v")  => parseVertex(line)
        case line if line.trim.startsWith("mtllib") => parseMaterialFile(line.replace("mtllib","").trim)
        case line if line.trim.startsWith("usemtl") => Unit //TODO set current material
        case line if line.trim.startsWith("f")  => parseFace(line)
        case line                               => logger.error(s"ERROR: Cannot parse this line: <$line>")
    })

    fileReadSuccessfully match {
      case Failure(f) => logger.error(s"Could not read object $filename, skipping!, $f")
                         Seq.empty

      case Success(_) => if(vertices.isEmpty) Seq.empty else createTriangles
    }

  }

  private def createTriangles = {
    val (coordinates_x, coordinates_y, coordinates_z) =
      (vertices.map(_.x), vertices.map(_.y), vertices.map(_.z))
    val scalingFactor = maxSide / Seq(
      coordinates_x.max - coordinates_x.min,
      coordinates_y.max - coordinates_y.min,
      coordinates_z.max - coordinates_z.min
    ).max

    val currentCenter = Vector3(
      (coordinates_x.max - coordinates_x.min) / 2 + coordinates_x.min,
      (coordinates_y.max - coordinates_y.min) / 2 + coordinates_y.min,
      (coordinates_z.max - coordinates_z.min) / 2 + coordinates_z.min
    )

    val height = (coordinates_y.max - coordinates_y.min) * scalingFactor
    val newCenter = centerBottom.copy(y = centerBottom.y + height / 2)

    for (i <- vertices.indices.par)
      vertices(i) = transformVertex(vertices(i), currentCenter, newCenter, scalingFactor, rotation)

    for (i <- normals.indices.par)
      normals(i) = transformNormal(normals(i), rotation).normalized

    val mat: String = material.getOrElse(DEFAULT_MATERIAL.name)

    val ts = triangles.map {
          case (a, b, c) =>

            val norms: Option[Seq[Vector3]] = for{
              an <- a.normal.flatMap(normals.lift)
              bn <- b.normal.flatMap(normals.lift)
              cn <- c.normal.flatMap(normals.lift)
            } yield Seq(an, bn, cn)

            val texCoords: Option[Seq[(Double, Double)]] = for{
              ta <- a.texture.flatMap(textureCoordinates.lift)
              tb <- b.texture.flatMap(textureCoordinates.lift)
              tc <- c.texture.flatMap(textureCoordinates.lift)
            } yield Seq(ta,tb,tc)

            Triangle(vertices(a.vertex-1), vertices(b.vertex-1), vertices(c.vertex-1), mat, norms, texCoords)
        }

      logger.info(
      s"Object read with ${vertices.size} vertices, ${normals.size} normals, " +
        s"${textureCoordinates.size} texture coordinates and ${triangles.size} triangles"
    )
    ts.toList
  }

  private def parseTextureCoordinate(line: String) =
    line.split("\\s+").slice(1, 3) match {
       case Array(u,v) =>  textureCoordinates += ((u.toDouble, v.toDouble))
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
    s.trim.split("/").filter(_ != "").map(_.trim.toInt)      //Case class (vertex,tecxture, normal)

  private[scene] case class FaceVertex(vertex: Int, texture:Option[Int], normal: Option[Int]){}
  object  FaceVertex{
    def fromString(s: String): Option[FaceVertex] = s.trim.split("/").map(x => Try(x.toInt).toOption) match {
           case Array(Some(v), t, n) =>  Some(FaceVertex(v,t,n))
           case _ => logger.warn(s"Could not parse face from line $s")
                     None
         }
  }

  private def parseFace(line: String) = {
    line.split("\\s+").slice(1, 5) match {
      case Array(a, b, c, d) =>
        (for{
          fa <- FaceVertex.fromString(a)
          fb <- FaceVertex.fromString(b)
          fc <- FaceVertex.fromString(c)
          fd <- FaceVertex.fromString(d)
        } yield ((fa, fb, fc),(fa, fc, fd))
        ).map(ts => triangles ++= Seq(ts._1,ts._2) )
      case Array(a, b, c) =>
        (for {
          fa <- FaceVertex.fromString(a)
          fb <- FaceVertex.fromString(b)
          fc <- FaceVertex.fromString(c)
        } yield (fa,fb,fc)).map( triangles += _ )
    }
  }

  private def parseMaterialFile(materialFilename : String): Unit = {
    println(s"\n\nreading $materialFilename from $pathPrefix\n\n")
  }

}
