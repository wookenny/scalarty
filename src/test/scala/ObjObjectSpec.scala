import math.breeze.VectorBreeze3
import math.breeze.VectorBreeze3._
import math.Triangle
import org.specs2.Specification
import org.specs2.mock.Mockito
import scene.ObjObject

import scala.io.BufferedSource

class ObjObjectSpec extends Specification with Mockito {

  override def is = s2"""
    An ObjObject should
      parse triangles $testParseTriangles
      parse quads $testParseQuads
      parse normals $testParseNormals
      translate and scale to desired bounding box $testScalingAndTranslation
      rotate vertices and normals after fitting to bounding box $testRotation
      set given material $testSetMaterial
    """

//TODO test rotation of normals

  val mockedSource = mock[BufferedSource]
  implicit val reader: String => BufferedSource = String => mockedSource
  val filename = "objFilename"

  val testParseTriangles = {
    val objFile = ObjObject(filename, center = ONE, maxSide = 2, rotation = 0, None)
    val objString = """|# Test ObjFile
                       |g triangle
                       |
                       |v 0 0 0
                       |v 2.00 2.000 2.0
                       |v 2.000000 0 0
                       |f 1 2 3 """.stripMargin
    mockedSource.getLines returns objString.lines

    val triangles: Seq[Triangle] = objFile.getTriangles
    val triangle = Triangle(ZERO, VectorBreeze3(2, 2, 2), VectorBreeze3(2, 0, 0))
    triangles should contain(exactly(triangle))
  }

  val testParseQuads = {
    val objFile = ObjObject(filename, center = ONE, maxSide = 2, rotation = 0, None)
    val objString = """|# Test file containing a quad
                       |v 0 0 0
                       |xxx not defined, needs to be skipped
                       |v 2.00 2.000 2.0
                       |v 2.000000 0 0
                       |v 2.000000 2 0
                       |f 1 2 3 4""".stripMargin
    mockedSource.getLines returns objString.lines

    val triangles: Seq[Triangle] = objFile.getTriangles
    val triangle1 = Triangle(ZERO, VectorBreeze3(2, 2, 2), VectorBreeze3(2, 0, 0))
    val triangle2 = Triangle(ZERO, VectorBreeze3(2, 0, 0), VectorBreeze3(2, 2, 0))

    triangles should contain(exactly(triangle1, triangle2))
  }

  val testParseNormals = {
    val objFile = ObjObject(filename, center = ONE, maxSide = 2, rotation = 0, None)
    val objString = """|# Test file containing a triangles and some normals
                       |v 0 0 0
                       |v 2.00 2.000 2.0
                       |v 2.000000 0 0
                       |v 2.000000 2 0
                       |vn 1 0 0
                       |vn 0 1 0
                       |vn 0 0 2
                       |vn 1 1 1
                       |f 1//3 2/121212/2 3/23/1 """.stripMargin
    mockedSource.getLines returns objString.lines

    val triangles: Seq[Triangle] = objFile.getTriangles

    val normals = Seq(Z, Y, X)
    val triangle1 =
      Triangle(ZERO, VectorBreeze3(2, 2, 2), VectorBreeze3(2, 0, 0), normals = Some(normals))
    triangles should contain(exactly(triangle1))
  }

  val testSetMaterial = {
    val mat = "SparklingMaterial"
    val objFile =
      ObjObject(filename, center = ONE, maxSide = 2, rotation = 0, material = Some(mat))
    val objString = """|# Test ObjFile
                       |v 0 0 0
                       |v 2.00 2.000 2.0
                       |v 2.000000 0 0
                       |f 1 2 3 """.stripMargin
    mockedSource.getLines returns objString.lines

    val triangles: Seq[Triangle] = objFile.getTriangles
    val triangle =
      Triangle(ZERO, VectorBreeze3(2, 2, 2), VectorBreeze3(2, 0, 0), mat)
    triangles should contain(exactly(triangle))
  }

  val testScalingAndTranslation = {
    val objFile = ObjObject(filename, center = VectorBreeze3(2, 2, 2), maxSide = 1, rotation = 0, None)
    val objString = """|# Test ObjFile
                       |v 0 0 0
                       |v 2 2 2
                       |v 2 0 0
                       |f 1 2 3 """.stripMargin
    mockedSource.getLines returns objString.lines

    val triangles: Seq[Triangle] = objFile.getTriangles
    val triangle = Triangle(VectorBreeze3(1.5, 1.5, 1.5), VectorBreeze3(2.5, 2.5, 2.5), VectorBreeze3(2.5, 1.5, 1.5))
    triangles should contain(exactly(triangle))
  }

  val testRotation = {
    val objFile = ObjObject(filename, center = VectorBreeze3(2, 2, 2), maxSide = 1, rotation = 180, None)
    val objString = """|# Test ObjFile
                       |v 0 1 0
                       |v 2 0 2
                       |v 2 0 0
                       |f 1 2 3 """.stripMargin
    mockedSource.getLines returns objString.lines

    val triangles: Seq[Triangle] = objFile.getTriangles
    val triangle =
      Triangle(VectorBreeze3(2.5, 2.25, 2.5), VectorBreeze3(1.5, 1.75, 1.5), VectorBreeze3(1.5, 1.75, 2.5))
    triangles should contain(exactly(triangle))
  }
}
