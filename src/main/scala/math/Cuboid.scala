package math

import java.lang.Math.{cos, sin}

import breeze.linalg.{DenseMatrix, DenseVector}
import math.Cuboid.Dimension.Dimension

sealed case class Cuboid(center: Vector3,
                         sideLenghts : Vector3,
                         rotation: (Double,Double,Double)
                        ) extends Shape{

  import Cuboid._, Dimension._

  private val vertices =  {
    val x = sideLenghts.x
    val y = sideLenghts.y
    val z = sideLenghts.z
    Seq(Vector3(x,0,0),Vector3(-1*x,0,0),
        Vector3(0,y,0),Vector3(0,-1*y,0),
        Vector3(0,0,z), Vector3(0,0,-1*z)).map( rotate(_,rotation) + center)

  }

  private val unrotated = AABB(center.x-sideLenghts.x, center.x+sideLenghts.x,
                               center.y-sideLenghts.y, center.y+sideLenghts.y,
                               center.z-sideLenghts.z, center.z+sideLenghts.z)

  private val rotMatrix = {

    Seq(rotation._1,rotation._2,rotation._3)
      .zip(Seq(X, Y, Z))
      .map{case (angle,d) => rotationMatrix(angle,d)}
      .reduce(_ *:* _)

  }

  override def intersect(r: Ray) = ??? //rotate ray, test against unrotated and rotate resulting normals

  override def intersect(r: Ray, maxDist: Double) = ???

  override lazy val boundingBox = AABB(minX,maxX,minY,maxY,minZ,maxZ)

  override val midpoint = center

  override lazy val minX = vertices.map(_.x).min

  override lazy val minY = vertices.map(_.y).min

  override lazy val minZ = vertices.map(_.z).min

  override lazy val maxX = vertices.map(_.x).max

  override lazy val maxY = vertices.map(_.y).max

  override lazy val maxZ = vertices.map(_.z).max


  private def rotate(point: Vector3, rotation: (Double,Double,Double), center: Vector3 = Vector3.ZERO) = {
    val r = rotMatrix * DenseVector(point.x, point.y, point.z)
    Vector3(r(0),r(1),r(2))
  }
}

object Cuboid{

  object Dimension extends Enumeration {
   type Dimension = Value
    val X = Value(0)
    val Y = Value(1)
    val Z = Value(2)
  }

  private def rotationMatrix(angle: Double, d: Dimension) = {
    import Math.π, Dimension._
    val rotMatrix = DenseMatrix.zeros[Double](3,3)

    val s: Double = sin(2 * π *angle / 360)
    val c: Double = cos(2 * π *angle / 360)

    d match {
      case X => {
        rotMatrix(0,::) := DenseVector(1d, 0d, 0d).t
        rotMatrix(1,::) := DenseVector(0d,  c,  s).t
        rotMatrix(2,::) := DenseVector(0d,- s,  c).t
      }
      case Y => {
        rotMatrix(0,::) := DenseVector(c,  0d, -s).t
        rotMatrix(1,::) := DenseVector(0d, 1d, 0d).t
        rotMatrix(2,::) := DenseVector(s,  0d,  c).t
      }
      case Z => {
        rotMatrix(0,::) := DenseVector( c,  s,  0d).t
        rotMatrix(1,::) := DenseVector(-s,  c,  0d).t
        rotMatrix(2,::) := DenseVector( 0d, 0d, 1d).t
      }
    }
    rotMatrix
  }

}
