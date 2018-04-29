package math

import material.Material.DEFAULT_MATERIAL
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

    val dirfrac =
      Vector3(1 / r.direction.x, 1 / r.direction.y, 1 / r.direction.z)

    val t1 = (x_min - r.origin.x) * dirfrac.x
    val t2 = (x_max - r.origin.x) * dirfrac.x

    val t3 = (y_min - r.origin.y) * dirfrac.y
    val t4 = (y_max - r.origin.y) * dirfrac.y

    val t5 = (z_min - r.origin.z) * dirfrac.z
    val t6 = (z_max - r.origin.z) * dirfrac.z

    val distances = Seq((t1, t2), (t3, t4), (t5, t6))
    val (tmin, normal) = distances
      .zip(Seq(Vector3.X, Vector3.Y, Vector3.Z))
      .map { case ((x, y), n) => if (x < y) (x, n * (-1)) else (y, n) }
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
    val dirfrac =
      Vector3(1 / r.direction.x, 1 / r.direction.y, 1 / r.direction.z)

    val t1 = (x_min - r.origin.x) * dirfrac.x
    val t2 = (x_max - r.origin.x) * dirfrac.x

    val t3 = (y_min - r.origin.y) * dirfrac.y
    val t4 = (y_max - r.origin.y) * dirfrac.y

    val t5 = (z_min - r.origin.z) * dirfrac.z
    val t6 = (z_max - r.origin.z) * dirfrac.z

    val distances = Seq((t1, t2), (t3, t4), (t5, t6))
    val tmin = distances.map { case (x, y)         => x min y }.max
    val tmax: Double = distances.map { case (x, y) => x max y }.min
    if (tmax < EPS || tmin > tmax)
      false
    else tmin < maxDist
  }

  override def boundingBox: AABB = this

  lazy val midpoint: Vector3 =
    Vector3((x_max + x_min) / 2, (y_max + y_min) / 2, (z_max + z_min) / 2)

  override def minX: Double = x_min

  override def minY: Double = y_min

  override def minZ: Double = z_min

  override def maxX: Double = x_max

  override def maxY: Double = y_max

  override def maxZ: Double = z_max

  def contains(vec: Vector3): Boolean =
    x_min <= vec.x && vec.x <= x_max &&
      y_min <= vec.y && vec.y <= y_max &&
      z_min <= vec.z && vec.z <= z_max

  lazy val area: Double =
    2 * ((x_max - x_min) * (y_max - y_min) + (x_max - x_min) * (z_max - z_min) * (y_max - y_min) * (z_max - z_min))

  def union(otherAABB: AABB): AABB = AABB.union(this, otherAABB)

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
  val unrestricted = AABB(Double.MinValue,
                          Double.MaxValue,
                          Double.MinValue,
                          Double.MaxValue,
                          Double.MinValue,
                          Double.MaxValue)
}
