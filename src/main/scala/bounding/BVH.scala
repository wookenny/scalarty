package bounding

import com.typesafe.scalalogging.LazyLogging
import math.{AABB, Ray, Shape}
import renderer.Hit


abstract class Node{
  def getDepth : Int
  def closestHit(ray: Ray) : Option[Hit]
  val boundingBox:  Option[AABB]

  def countNodes : Int

  def intersectingBoundingBox(ray: Ray) : Boolean =  boundingBox match {
      case None       => false
      case Some(aabb) => (aabb contains ray.origin) || aabb.intersect(ray,Float.MaxValue)
    }

  def intersectBoundingBox(ray: Ray) : Option[Float] =  boundingBox match {
    case None       => None
    case Some(aabb) => aabb.intersect(ray).map(_.distance)
  }
}

final case class InnerNode(boundingBox: Option[AABB], children:  Seq[Node], depth: Int) extends Node{
  override def closestHit(ray: Ray)  = None
  override def getDepth = children.map{  _ getDepth }.max

  override def countNodes: Int = 1+children.foldLeft(0)(_ + _.countNodes)
}

final case class Leaf(boundingBox: Option[AABB], shapes:  Seq[Shape], depth: Int) extends Node{

  override def closestHit(ray: Ray) : Option[Hit] = shapes flatMap { s => s intersect ray } match {
        case Nil => None
        case xs => Some(xs.minBy(_.distance))
  }

  override def getDepth = depth

  override def countNodes: Int = 1
}



case class BVH(shapes: Seq[Shape], leaf_node_limit : Int = 4) extends ShapeContainer with LazyLogging {
  val root : Node = buildBVH(shapes,0)

  logger.info(s"Bild BVH tree with ${size} primitives, ${numNodes} nodes and depth $depth")

  override def size = shapes.size

  lazy val numNodes : Int = root.countNodes
  lazy val depth : Int =  root.getDepth


  private final case class Candidate(node: Node, distance: Float)

  private def getBoundingBox(shapes: Seq[Shape]) : Option[AABB] = shapes.map(_.boundingBox).reduceOption(AABB.union)


  private def getSplitFunction(shapes: Seq[Shape], aabb: AABB) : Shape => Boolean = {
    (aabb.x_max-aabb.x_min, aabb.y_max-aabb.y_min, aabb.z_max-aabb.z_min) match {
      case (x,y,z) if x >= y && x >= z => (s:Shape) => s.midpoint.x < shapes.map(_.midpoint.x).sorted.toSeq(shapes.size/2)
      case (x,y,z) if y >= x && y >= z => (s:Shape) => s.midpoint.y < shapes.map(_.midpoint.y).sorted.toSeq(shapes.size/2)
      case _                           => (s:Shape) => s.midpoint.z < shapes.map(_.midpoint.z).sorted.toSeq(shapes.size/2)
    }
  }

  private def splitPrimitives(shapes: Seq[Shape], aabb: Option[AABB]) :  Option[(Seq[Shape],Seq[Shape])] = {
    //primitive split: select max dim and choose mean midpoint

    if(aabb.isEmpty || shapes.size <= leaf_node_limit)
      None
    else {
      val splitFunction = getSplitFunction(shapes,aabb.get)
      shapes.partition(splitFunction) match {
        case (a, b) if (a.size min b.size) <= leaf_node_limit => None
        case (a, b) => Some(a, b)
      }
    }
  }

  private def buildBVH(shapes: Seq[Shape], depth: Int) : Node = {
    val aabb = getBoundingBox(shapes)
    val split = splitPrimitives(shapes, aabb)
        split match {
          case None        =>  Leaf(aabb, shapes, depth)
          case Some((a:Seq[Shape],b:Seq[Shape])) =>
            InnerNode(aabb, Seq( buildBVH(a,depth+1), buildBVH(b,depth+1)), depth)
        }


  }

  private def getCandidatesWithCloseHits(node : InnerNode, ray: Ray, closestHit: Option[Hit]): Seq[Candidate] = {

    val candidates = node.children map { (node: Node) => node.intersectBoundingBox(ray) match {
            case None        => Candidate(node, Float.MaxValue)
            case Some(dist)  => Candidate(node, dist)
          }
        }

    candidates filter { (candidate: Candidate) => candidate.distance < (closestHit match {
                                                                        case None => Float.MaxValue
                                                                        case Some(hit) => hit.distance
                                                                      })
    }

  }

  override def intersect(ray: Ray): Option[Hit] = {
    if ( !root.intersectingBoundingBox(ray)) {
      None
    }else {
      var closestHit : Option[Hit] = None
      val nodeStack = scala.collection.mutable.Queue[Node]()
      nodeStack += root
      while (!nodeStack.isEmpty) {
          nodeStack.dequeue match {
          case node: InnerNode=>
            val nodeBoundingBoxHits : Seq[Candidate] = getCandidatesWithCloseHits(node, ray, closestHit)
              //add to stack
            nodeStack ++= nodeBoundingBoxHits.sorted(Ordering.by((c:Candidate) => c.distance)).map(_.node)
          case node: Leaf =>

            node.closestHit(ray) match {
              case Some(hit) if closestHit.isEmpty ||  hit.distance < closestHit.get.distance  =>
                  closestHit = Some(hit)
              case _ =>  Unit
            }
        }

      }
      closestHit
    }
  }
  override def intersectionTest(ray: Ray, maxDist: Float): Boolean =
    //TODO: implement better
    intersect(ray) match {
      case Some(hit) if hit.distance <= maxDist => true
      case _ => false
    }

}
/*
object BVH{
  val shapeIntersectionCost: Int
}*/