package bounding

import cats.data.NonEmptyVector
import color.RGB
import com.typesafe.scalalogging.LazyLogging
import material.SingleColorMaterial
import math._
import renderer.Hit
import support.Config
import support.Util._

abstract class Node {
  def getDepth: Int
  def closestHit(ray: Ray): Option[Hit]
  val boundingBox: AABB

  def countNodes: Int

  def intersectingBoundingBox(ray: Ray): Boolean =
    (boundingBox contains ray.origin) || boundingBox.intersect(ray, Double.MaxValue)

  def intersectBoundingBox(ray: Ray): Option[Double] =
    boundingBox.intersect(ray).map(_.distance)
}

private final case class Candidate(node: Node, distance: Double)

final case class EmtyRootNode() extends Node {
  override val getDepth: Int = 0

  override def closestHit(ray: Ray): Option[Hit] = None

  override val boundingBox: AABB = AABB.Empty

  override val countNodes: Int = 0
}

final case class InnerNode(boundingBox: AABB, children: Vector[Node], depth: Int) extends Node {
  override def closestHit(ray: Ray) = None
  override lazy val getDepth: Int = children.map { _ getDepth }.max

  override def countNodes: Int = 1 + children.foldLeft(0)(_ + _.countNodes)
}

final case class Leaf(boundingBox: AABB, shapes: Seq[Shape], depth: Int) extends Node {

  override def closestHit(ray: Ray): Option[Hit] =
    shapes flatMap { s =>
      s intersect ray
    } match {
      case Nil => None
      case xs  => Some(xs.minBy(_.distance))
    }

  override val getDepth: Int = depth

  override val countNodes: Int = 1
}

case class BVH(shapes: Vector[Shape], leafNodeLimit: Int = 12, splitSAH: Boolean = true)(
    implicit config: Config
) extends ShapeContainer
    with LazyLogging {
  logger.info(s"Building BVH with sah=$splitSAH and leafNodeLimit=$leafNodeLimit...")

  implicit val log: String => Unit = s => logger.info(s)

  var nodesCreated: Int = 0
  val root: Node = NonEmptyVector.fromVector(shapes) match {
    case None =>
      logger.info("Scene does not contain shapes!")
      EmtyRootNode()
    case Some(nonEmptyVector) => time("Building BVH took") { buildBVH(nonEmptyVector, 0) }
  }

  logger.info(s"Build BVH tree with $size primitives, $numNodes nodes and depth $depth")

  override lazy val size: Int = shapes.size

  Shape.materialMap = Shape.materialMap + (BVH.leafMaterial.name -> BVH.leafMaterial)

  lazy val numNodes: Int = root.countNodes
  lazy val depth: Int = root.getDepth

  private def getBoundingBox(shapes: NonEmptyVector[Shape]): NonEmptyAABB =
    AABB.wrappingShapes(shapes)

  private def splitPrimitives(
      shapes: NonEmptyVector[Shape],
      aabb: NonEmptyAABB
  ): Option[(NonEmptyVector[Shape], NonEmptyVector[Shape])] = {
    //primitive split: select max dim and choose mean midpoint
    if (shapes.length <= leafNodeLimit)
      None
    else {
      val (left, right) =
        if (splitSAH)
          getSplitSAH(shapes, aabb)
        else getSplit(shapes, aabb)

      (NonEmptyVector.fromVector(left), NonEmptyVector.fromVector(right)) match {
        case (Some(a), Some(b)) => Some(a, b)
        case _                  => None
      }

    }
  }

  private def getSplit(
      shapes: NonEmptyVector[Shape],
      aabb: NonEmptyAABB
  ): (Vector[Shape], Vector[Shape]) = {
    import cats.implicits._
    val split: NonEmptyVector[Shape] =
      (aabb.lenghtX, aabb.lenghtY, aabb.lenghtZ) match {
        case (x, y, z) if x >= y && x >= z => shapes.sortBy(_.midpoint.x)
        case (x, y, z) if y >= x && y >= z => shapes.sortBy(_.midpoint.y)
        case _                             => shapes.sortBy(_.midpoint.z)
      }

    val splitIndex = split.length / 2
    (split.toVector.take(splitIndex), split.toVector.drop(splitIndex))
  }

  private def getSplitSAH(
      shapes: NonEmptyVector[Shape],
      aabb: NonEmptyAABB
  ): (Vector[Shape], Vector[Shape]) = {

    import cats.implicits._
    val split: NonEmptyVector[Shape] =
      (aabb.x_max - aabb.x_min, aabb.y_max - aabb.y_min, aabb.z_max - aabb.z_min) match {
        case (x, y, z) if x >= y && x >= z => shapes.sortBy(_.midpoint.x)
        case (x, y, z) if y >= x && y >= z => shapes.sortBy(_.midpoint.y)
        case _                             => shapes.sortBy(_.midpoint.z)
      }

    val splitIndex = getOptimalSplit(split, aabb.area)
    (split.toVector.take(splitIndex), split.toVector.drop(splitIndex))

  }

  def getOptimalSplit(shapes: NonEmptyVector[Shape], wholeSurfaceArea: Double): Int = {

    val forw = BVH.findBoundingBoxesAreas(shapes)
    val backw = BVH.findBoundingBoxesAreas(shapes.reverse).reverse

    val splitcost: Int => Double = i =>
      BVH.CostAABBIntersection +
        (forw(i) / wholeSurfaceArea) * i * BVH.CostShapeIntersection +
        (backw(i + 1) / wholeSurfaceArea) * (backw.size - i) * BVH.CostShapeIntersection

    val costNoSplit: Vector[(Int, Double)] = Vector((0, shapes.length * BVH.CostShapeIntersection))
    val costs = costNoSplit ++ (1 until forw.size - 1).par.map(i => (i, splitcost(i)))

    costs.minBy(_._2)._1
  }

  private def buildBVH(shapes: NonEmptyVector[Shape], depth: Int): Node = {
    val aabb: NonEmptyAABB = getBoundingBox(shapes)
    val split = splitPrimitives(shapes, aabb)
    (split, aabb) match {
      case (None, box) if config.showBvHLeaves =>
        Leaf(aabb, shapes.toVector :+ box.copy(material = BVH.leafMaterial.name), depth)
      case (None, _) => Leaf(aabb, shapes.toVector, depth)
      case (Some((a, b)), _) =>
        InnerNode(aabb, Vector(buildBVH(a, depth + 1), buildBVH(b, depth + 1)), depth)
    }
  }

  private def getCandidatesWithCloseHits(
      node: InnerNode,
      ray: Ray,
      closestHitDist: Double
  ): Vector[Candidate] = {
    val candidates = node.children map { node: Node =>
      node.intersectBoundingBox(ray) match {
        case None       => Candidate(node, Double.MaxValue)
        case Some(dist) => Candidate(node, dist)
      }
    }
    candidates filter { _.distance < closestHitDist }

  }

  override def intersect(ray: Ray): Option[Hit] = {
    if (!root.intersectingBoundingBox(ray)) {
      None
    } else {
      var closestHit: Option[Hit] = None
      val nodeStack = scala.collection.mutable.Queue[Node]() //Replace by non-mutable
      nodeStack += root
      while (nodeStack.nonEmpty) {
        val closestHitDist = closestHit match {
          case None      => Double.MaxValue
          case Some(hit) => hit.distance
        }
        nodeStack.dequeue match {
          case node: InnerNode =>
            val nodeBoundingBoxHits: Vector[Candidate] =
              getCandidatesWithCloseHits(node, ray, closestHitDist)
            //remember closest aabb hit if this hould be shown
            if (BVH.showInnerNodes && nodeBoundingBoxHits.nonEmpty) {
              val aabbHitDist = nodeBoundingBoxHits.map(_.distance).min
              if (aabbHitDist < closestHitDist)
                closestHit = Some(
                  Hit(
                    distance = aabbHitDist,
                    position = ray.march(aabbHitDist),
                    originalNormal = Vector3.X, /*not needed for shading*/
                    material = BVH.innerNodeMaterial.getMat(Vector3.ZERO)
                  )
                )
            }
            //add to stack
            nodeStack ++= nodeBoundingBoxHits
              .sortBy((c: Candidate) => c.distance)
              .map(_.node)
          case node: Leaf =>
            node.closestHit(ray) match {
              case Some(hit) if hit.distance < closestHitDist =>
                closestHit = Some(hit)
              case _ => Unit
            }
        }

      }
      closestHit
    }
  }

  //TODO: is there a more efficient implemention using the fact, that there is a range bound?
  override def intersectionTest(ray: Ray, maxDist: Double): Boolean =
    intersect(ray) match {
      case Some(hit) if hit.distance <= maxDist => true
      case _                                    => false
    }

}

object BVH {
  private val showInnerNodes = false
  private val leafMaterial = SingleColorMaterial(
    "BVH_Leaf_Material",
    RGB.RED,
    ambient = 0.1,
    diffuse = 0,
    spec = 0,
    refractive = .9,
    n = 1
  )
  private val innerNodeMaterial = SingleColorMaterial(
    "BVH_Inner_Node_Material",
    RGB.YELLOW,
    ambient = 0.01,
    diffuse = 0,
    spec = 0,
    refractive = .99,
    n = 1
  )

  private val CostShapeIntersection: Int = 1
  private val CostAABBIntersection: Int = 7

  private def findBoundingBoxesAreas(shapes: NonEmptyVector[Shape]): Vector[Double] = {
    val (_, areaSizes) = shapes.tail.foldLeft((shapes.head.boundingBox, Vector.empty[Double])) {
      case ((aabb, areas), shape) =>
        val newBoundingBox: AABB = aabb union shape.boundingBox
        (newBoundingBox, areas :+ newBoundingBox.area)
    }

    areaSizes
  }

}
