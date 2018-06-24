package math

import java.lang.Math.{cos, sin}

import breeze.linalg.{DenseMatrix, DenseVector}
import math.Cuboid.Dimension.Dimension
import renderer.Hit

sealed case class Cuboid(
    center: Vector3,
    sideLengths: Vector3,
    rotation: Vector3,
    material: String = "DEFAULT_MATERIAL"
) extends Shape {

  import Cuboid._, Dimension._

  private val vertices: Seq[Vector3] = {
    val x = sideLengths.x
    val y = sideLengths.y
    val z = sideLengths.z
    Seq(
      Vector3(x, 0, 0),
      Vector3(-1 * x, 0, 0),
      Vector3(0, y, 0),
      Vector3(0, -1 * y, 0),
      Vector3(0, 0, z),
      Vector3(0, 0, -1 * z)
    ).map(rotatePoint(_) + center)

  }

  private val unrotated = AABB(
    center.x - sideLengths.x / 2,
    center.x + sideLengths.x / 2,
    center.y - sideLengths.y / 2,
    center.y + sideLengths.y / 2,
    center.z - sideLengths.z / 2,
    center.z + sideLengths.z / 2,
    material
  )

  private lazy val rotMatrix: DenseMatrix[Double] = {
    Seq(rotation.x, rotation.y, rotation.z)
      .zip(Seq(X, Y, Z))
      .map { case (angle, d) => rotationMatrix(angle, d) }
      .reduce(_ * _)
  }

  private lazy val inverseRotMatrix: DenseMatrix[Double] = {
    Seq(-rotation.x, -rotation.y, -rotation.z)
      .zip(Seq(X, Y, Z))
      .map { case (angle, d) => rotationMatrix(angle, d) }
      .reduce(_ * _)
  }

  override def intersect(r: Ray): Option[Hit] = {
    //println(s"rotation matrixes: \n positive\n\n$rotMatrix \n negative\n\n$inverseRotMatrix\n")
    //println(s"original ray: $r")
    val rr = rotateRay(r, center, rotateInverse = true)
    //println(s"rotated ray: $rr")
    val hit: Option[Hit] = unrotated.intersect(rr)
    //println(s"hit: position ${hit.map(_.position)}, normal ${hit.map(_.normal)}")
    val rotatedHit = hit.map { h =>
      h.copy(position = rotatePoint(h.position, center), originalNormal = rotatePoint(h.normal))
    }
    //println(s"rotated hit: position ${rotatedHit.map(_.position)}, normal ${rotatedHit.map(_.normal)}")
    rotatedHit
  }

  override def intersect(r: Ray, maxDist: Double): Boolean = {
    //rotate ray
    val rr = rotateRay(r, center, rotateInverse = true)
    unrotated.intersect(rr, maxDist) //no need to rotate back
  }

  override lazy val boundingBox = AABB(minX, maxX, minY, maxY, minZ, maxZ)

  override val midpoint = center

  override lazy val minX = vertices.map(_.x).min

  override lazy val minY = vertices.map(_.y).min

  override lazy val minZ = vertices.map(_.z).min

  override lazy val maxX = vertices.map(_.x).max

  override lazy val maxY = vertices.map(_.y).max

  override lazy val maxZ = vertices.map(_.z).max

  private def rotateRay(
      ray: Ray,
      center: Vector3 = Vector3.ZERO,
      rotateInverse: Boolean = false
  ): Ray = {
    val rotatedOrigin = rotatePoint(ray.origin, center, rotateInverse)
    val rotatedDirection = rotatePoint(ray.direction, Vector3.ZERO, rotateInverse)
    ray.copy(origin = rotatedOrigin, direction = rotatedDirection)
  }

  private def rotatePoint(
      point: Vector3,
      center: Vector3 = Vector3.ZERO,
      rotateInverse: Boolean = false
  ): Vector3 = {
    val matrix =
      if (rotateInverse)
        inverseRotMatrix
      else rotMatrix
    val r = matrix * DenseVector(point.x - center.x, point.y - center.y, point.z - center.z)
    Vector3(r(0), r(1), r(2)) + center
  }
}

object Cuboid {

  object Dimension extends Enumeration {
    type Dimension = Value
    val X = Value(0)
    val Y = Value(1)
    val Z = Value(2)
  }

  private def rotationMatrix(angle: Double, d: Dimension) = {
    import Math.π, Dimension._
    val rotMatrix = DenseMatrix.zeros[Double](3, 3)

    val s: Double = sin(2d * π * angle / 360)
    val c: Double = cos(2d * π * angle / 360)

    d match {
      case X => {
        rotMatrix(0, ::) := DenseVector(1d, 0d, 0d).t
        rotMatrix(1, ::) := DenseVector(0d, c, s).t
        rotMatrix(2, ::) := DenseVector(0d, -s, c).t
      }
      case Y => {
        rotMatrix(0, ::) := DenseVector(c, 0d, -s).t
        rotMatrix(1, ::) := DenseVector(0d, 1d, 0d).t
        rotMatrix(2, ::) := DenseVector(s, 0d, c).t
      }
      case Z => {
        rotMatrix(0, ::) := DenseVector(c, s, 0d).t
        rotMatrix(1, ::) := DenseVector(-s, c, 0d).t
        rotMatrix(2, ::) := DenseVector(0d, 0d, 1d).t
      }
    }
    //println(s"angle $angle with $d let to matrix \n\n$rotMatrix\n\n")
    rotMatrix
  }

}
