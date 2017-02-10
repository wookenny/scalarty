package bounding

import color.RGB
import com.typesafe.scalalogging.{LazyLogging, Logger}
import material.{Material, SingleColorMaterial, UnshadedColor}
import math.{AABB, Ray, Shape, Vector3}
import renderer.Hit
import support.Config
import support.Util._

import scala.annotation.tailrec
import scala.collection.mutable

abstract class Node {
  def getDepth: Int
  def closestHit(ray: Ray): Option[Hit]
  val boundingBox: Option[AABB]

  def countNodes: Int

  def intersectingBoundingBox(ray: Ray): Boolean = boundingBox match {
    case None => false
    case Some(aabb) =>
      (aabb contains ray.origin) || aabb.intersect(ray, Double.MaxValue)
  }

  def intersectBoundingBox(ray: Ray): Option[Double] = boundingBox match {
    case None => None
    case Some(aabb) => aabb.intersect(ray).map(_.distance)
  }
}

final case class InnerNode(boundingBox: Option[AABB],
                           children: Seq[Node],
                           depth: Int)
    extends Node {
  override def closestHit(ray: Ray) = None
  override def getDepth: Int = children.map { _ getDepth }.max

  override def countNodes: Int = 1 + children.foldLeft(0)(_ + _.countNodes)
}

final case class Leaf(boundingBox: Option[AABB],
                      shapes: Seq[Shape],
                      depth: Int)
    extends Node {

  override def closestHit(ray: Ray): Option[Hit] =
    shapes flatMap { s =>
      s intersect ray
    } match {
      case Nil => None
      case xs => Some(xs.minBy(_.distance))
    }

  override def getDepth: Int = depth

  override def countNodes: Int = 1
}

case class BVH(shapes: Seq[Shape], leaf_node_limit: Int = 20)(implicit config: Config)
    extends ShapeContainer
    with LazyLogging {
  logger.info("Building BVH ...")

  implicit val log: (String) => Unit = s => logger.info(s)

  var nodesCreated: Int = 0
  val root: Node = time("Building BVH took") { buildBVH(shapes, 0) }

  logger.info(
    s"Build BVH tree with ${size} primitives, ${numNodes} nodes and depth $depth")

  override def size = shapes.size

  Shape.materialMap = Shape.materialMap + (BVH.leafMaterial.name -> BVH.leafMaterial)

  lazy val numNodes: Int = root.countNodes
  lazy val depth: Int = root.getDepth

  private final case class Candidate(node: Node, distance: Double)

  private def getBoundingBox(shapes: Seq[Shape]): Option[AABB] =
    AABB.wrapping(shapes)

  private def splitPrimitives(
      shapes: Seq[Shape],
      aabb: Option[AABB]): Option[(Seq[Shape], Seq[Shape])] = {
    //primitive split: select max dim and choose mean midpoint
    if (aabb.isEmpty || shapes.size <= leaf_node_limit)
      None
    else {
      val split = getSplit(shapes, aabb.get)
      split match {
        case (a, b) if (a.size min b.size) == 0 /* leaf_node_limit*/ => None
        case (a, b) => Some(a, b)
      }
    }
  }

  private def getSplit(shapes: Seq[Shape],
                       aabb: AABB): (Seq[Shape], Seq[Shape]) = {
    val split: Seq[Shape] = (aabb.x_max - aabb.x_min,
                             aabb.y_max - aabb.y_min,
                             aabb.z_max - aabb.z_min) match {
      case (x, y, z) if x >= y && x >= z => shapes.sortBy(_.midpoint.x)
      case (x, y, z) if y >= x && y >= z => shapes.sortBy(_.midpoint.y)
      case _ => shapes.sortBy(_.midpoint.z)
    }

    val splitIndex = if (BVH.splitSAH) {
      val forw = BVH.findBoundingBoxes(split.toIndexedSeq)
      val backw = BVH.findBoundingBoxes(split.toIndexedSeq.reverse).reverse
      getOptimalSplit(forw, backw, aabb.area)
    } else
      split.size / 2

    (split.take(splitIndex), split.drop(splitIndex))
  }

  private def splitPrimitivesSAH(
      shapes: Seq[Shape],
      aabb: Option[AABB]): Option[(Seq[Shape], Seq[Shape])] = {

    if (shapes.size >= leaf_node_limit && aabb.isDefined) {
      val box = aabb.get
      val shapesSorted = Seq(shapes.sortBy(_.midpoint.x),
                             shapes.sortBy(_.midpoint.y),
                             shapes.sortBy(_.midpoint.z))
      val lengths = Seq(box.x_max - box.x_min,
                        box.y_max - box.y_min,
                        box.z_max - box.z_min)
      val dimension = lengths.indexOf(lengths.max)
      val (splitDimension, splitIndex, _) = (for {
        //dimension <- 0 to 2
        split <- shapes.indices
        costs = splitCost(shapesSorted(dimension), split, box)
      } yield (dimension, split, costs)).minBy(_._3)

      if (splitIndex == 0 || splitIndex == shapes.size - 1)
        None
      else {
        Option(shapesSorted(splitDimension).take(splitIndex),
               shapesSorted(splitDimension).drop(splitIndex))
      }
    } else {
      None
    }
  }

  private def splitCost(shapes: Seq[Shape], split: Int, aabb: AABB): Double = {
    val wholeSurfaceArea = aabb.area
    val nodesLeft = split + 1
    val nodesRight = shapes.size - split
    val areaLeft = getBoundingBox(shapes.slice(0, split)) match {
      case Some(box) => box.area
      case None => 0f
    }
    val areaRight = getBoundingBox(shapes.slice(split, shapes.size)) match {
      case Some(box) => box.area
      case None => 0f
    }
    if (nodesLeft == 0 || nodesRight == 0)
      shapes.size * BVH.CostShapeIntersection
    else
      BVH.CostAABBIntersection
    +(areaLeft / wholeSurfaceArea) * nodesLeft * BVH.CostShapeIntersection +
      (areaRight / wholeSurfaceArea) * nodesRight * BVH.CostShapeIntersection
  }

  def getOptimalSplit(forw: Seq[Double],
                      backw: Seq[Double],
                      wholeSurfaceArea: Double): Int = {
    require(forw.size == backw.size)

    var index = 0
    var cost: Double = shapes.size * BVH.CostShapeIntersection
    for (i <- 1 until forw.size - 1) {
      val splitcost: Double = BVH.CostAABBIntersection +
          (forw(i) / wholeSurfaceArea) * i * BVH.CostShapeIntersection +
          (backw(i + 1) / wholeSurfaceArea) * (backw.size - i) * BVH.CostShapeIntersection
      if (splitcost < cost) {
        index = i
        cost = splitcost
      }
    }
    index
  }

  private def buildBVH(shapes: Seq[Shape], depth: Int): Node = {
    val aabb = getBoundingBox(shapes)
    splitPrimitives(shapes, aabb) match {
      case None => if(BVH.showLeaves) {
        val box = aabb.get //TODO: do nicer, getOrElse
        Leaf(aabb, shapes :+ box.copy(material = BVH.leafMaterial.name) , depth)
      }else {
        Leaf(aabb, shapes, depth)
      }
      case Some((a: Seq[Shape], b: Seq[Shape])) =>
        InnerNode(aabb,
                  Seq(buildBVH(a, depth + 1), buildBVH(b, depth + 1)),
                  depth)
    }

  }

  private def getCandidatesWithCloseHits(node: InnerNode, ray: Ray, closestHitDist: Double): Seq[Candidate] = {
    val candidates = node.children map { (node: Node) =>
      node.intersectBoundingBox(ray) match {
        case None => Candidate(node, Double.MaxValue)
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
      val nodeStack = scala.collection.mutable.Queue[Node]()
      nodeStack += root
      while (nodeStack.nonEmpty) {
        val closestHitDist = closestHit match {
          case None => Double.MaxValue
          case Some(hit) => hit.distance
        }
        nodeStack.dequeue match {
          case node: InnerNode =>
            val nodeBoundingBoxHits: Seq[Candidate] =
              getCandidatesWithCloseHits(node, ray, closestHitDist)
            //remember closest aabb hit if this hould be shown
            if(BVH.showInnerNodes && nodeBoundingBoxHits.nonEmpty){
              val aabbHitDist = nodeBoundingBoxHits.map(_.distance).min
              if(aabbHitDist<closestHitDist)
                closestHit = Some( Hit( distance = aabbHitDist,
                                        position = ray.march(aabbHitDist),
                                        normal = Vector3.X, /*not needed for shading*/
                                        color  = BVH.innerNodeMaterial.getMat(Vector3.ZERO)))
            }
            //add to stack
            nodeStack ++= nodeBoundingBoxHits
              .sortBy((c: Candidate) => c.distance)
              .map(_.node)
          case node: Leaf =>
            node.closestHit(ray) match {
              case Some(hit)
                  if hit.distance < closestHitDist =>
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
      case _ => false
    }

}

object BVH {
  private val showLeaves = false
  private val showInnerNodes = false
  private val leafMaterial = SingleColorMaterial("BVH_Leaf_Material",RGB.RED, ambient =  0.1, diffuse = 0, spec = 0, refractive = .9, n = 1)
  private val innerNodeMaterial = SingleColorMaterial("BVH_Inner_Node_Material",RGB.YELLOW, ambient =  0.01, diffuse = 0, spec = 0, refractive = .99, n = 1)
  val splitSAH = false


  private val CostShapeIntersection: Int = 1
  private val CostAABBIntersection: Int = 3

  private def findMax(l: Seq[Int]): Seq[Int] = l match {
    case Nil => Seq.empty[Int]
    case (head :: tail) => findMaxRec(tail, head, Nil)
  }

  @tailrec
  private def findMaxRec(l: Seq[Int], value: Int, acc: Seq[Int]): Seq[Int] =
    l match {
      case Nil => acc
      case (head :: tail) =>
        val v: Int = value max head; findMaxRec(tail, v, acc :+ v)
    }

  private def findBoundingBoxes(l: Seq[Shape]): Seq[Double] = l match {
    case Nil => Seq.empty
    case x =>
      findBoundingBoxesRec(x.tail,
                           x.head.boundingBox,
                           new mutable.ArraySeq[Double](l.size),
                           0)
  }

  @tailrec
  private def findBoundingBoxesRec(l: Seq[Shape],
                                   value: AABB,
                                   acc: mutable.ArraySeq[Double],
                                   position: Int): Seq[Double] = l match {
    case Nil => acc
    case x if position == l.size => acc
    case x =>
      val v = value union x(position).boundingBox
      acc(position) = v.area
      findBoundingBoxesRec(x, v, acc, position + 1)
  }


}
