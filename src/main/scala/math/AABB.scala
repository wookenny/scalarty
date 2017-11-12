package math

import material.Material.DEFAULT_MATERIAL
import math.breeze.VectorBreeze3
import renderer.Hit

sealed case class AABB(x_min: Double,
                       x_max: Double,
                       y_min: Double,
                       y_max: Double,
                       z_min: Double,
                       z_max: Double,
                       material: String = DEFAULT_MATERIAL.name)
    extends Shape {
  import Math._
  require(x_min <= x_max)
  require(y_min <= y_max)
  require(z_min <= z_max)

  override def intersect(r: Ray): Option[Hit] = {

    val dirfrac: VectorBreeze3 = 1d/r.direction

    val t1 = (x_min - r.origin(0)) * dirfrac(0)
    val t2 = (x_max - r.origin(0)) * dirfrac(0)

    val t3 = (y_min - r.origin(1)) * dirfrac(1)
    val t4 = (y_max - r.origin(1)) * dirfrac(1)

    val t5 = (z_min - r.origin(2)) * dirfrac(2)
    val t6 = (z_max - r.origin(2)) * dirfrac(2)

    val distances = Seq((t1, t2), (t3, t4), (t5, t6))
    val (tmin, normal) = distances
      .zip(Seq(VectorBreeze3.X, VectorBreeze3.Y, VectorBreeze3.Z))
      .map { case ((x, y), n: VectorBreeze3) => if (x < y) (x, -n) else (y, n) }
      .maxBy(_._1)
    val tmax: Double = distances.map { case (x, y) => x max y }.min

    if (tmax < EPS || tmin > tmax) {
      None
    } else {
      //use tmax if we are inside the AABB
      val t = if (tmin < 0) tmax else tmin
      val pos = r.march(t)
      Some(Hit(t, pos, normal, Shape.getMaterial(material, pos)))
    }
  }

  //TODO: should only generate needed data, not too much in advance
  override def intersect(r: Ray, maxDist: Double): Boolean = {
    val dirfrac : VectorBreeze3 = 1d / r.direction

    val t1 = (x_min - r.origin(0)) * dirfrac(0)
    val t2 = (x_max - r.origin(0)) * dirfrac(0)

    val t3 = (y_min - r.origin(1)) * dirfrac(1)
    val t4 = (y_max - r.origin(1)) * dirfrac(1)

    val t5 = (z_min - r.origin(2)) * dirfrac(2)
    val t6 = (z_max - r.origin(2)) * dirfrac(2)

    val distances = Seq((t1, t2), (t3, t4), (t5, t6))
    val tmin = distances.map { case (x, y) => x min y }.max
    val tmax: Double = distances.map { case (x, y) => x max y }.min
    if (tmax < EPS || tmin > tmax)
      false
    else tmin < maxDist
  }

  override def boundingBox: AABB = this

  override def midpoint: VectorBreeze3 =
    VectorBreeze3.from((x_max + x_min) / 2, (y_max + y_min) / 2, (z_max + z_min) / 2)

  override def minX: Double = x_min

  override def minY: Double = y_min

  override def minZ: Double = z_min

  override def maxX: Double = x_max

  override def maxY: Double = y_max

  override def maxZ: Double = z_max

  def contains(vec: VectorBreeze3): Boolean =
    x_min <= vec(0) && vec(0) <= x_max &&
      y_min <= vec(1) && vec(1) <= y_max &&
      z_min <= vec(2) && vec(2) <= z_max

  def area: Double =
    2 * ((x_max - x_min) * (y_max - y_min) + (x_max - x_min) * (z_max - z_min) * (y_max - y_min) * (z_max - z_min))

  def union(otherAABB: AABB) = AABB.union(this, otherAABB)

}

object AABB {

  def wrapping(shapes: Seq[Shape]): Option[AABB] = {
    if (shapes.isEmpty)
      None
    else
      Some(
        AABB(
          shapes.tail.foldLeft(shapes.head.minX)(_ min _.minX) - Math.EPS,
          shapes.tail.foldLeft(shapes.head.maxX)(_ max _.maxX) + Math.EPS,
          shapes.tail.foldLeft(shapes.head.minY)(_ min _.minY) - Math.EPS,
          shapes.tail.foldLeft(shapes.head.maxY)(_ max _.maxY) + Math.EPS,
          shapes.tail.foldLeft(shapes.head.minZ)(_ min _.minZ) - Math.EPS,
          shapes.tail.foldLeft(shapes.head.maxZ)(_ max _.maxZ) + Math.EPS
        )
      )
  }

  def union(a: AABB, b: AABB): AABB = union(Seq(a, b))
  def union(boxes: Iterable[AABB]): AABB = {
    if (boxes.isEmpty)
      AABB.empty
    else
      boxes.reduce { (a: AABB, b: AABB) =>
        AABB(a.x_min min b.x_min,
             a.x_max max b.x_max,
             a.y_min min b.y_min,
             a.y_max max b.y_max,
             a.z_min min b.z_min,
             a.z_max max b.z_max)
      }
  }
  //TODO: empty is not really an empty element and mightbreak
  val empty = AABB(0, 0, 0, 0, 0, 0)
}
