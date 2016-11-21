package bounding

import math.{AABB, Ray, Shape}
import renderer.Hit

final case class Node(boundingBox: AABB, shapes: Seq[Shape], children: Seq[Node])

class BVH(val shapes: Seq[Shape]) {

  private def buildBVH(shapes: Seq[Shape]) = ???

  private lazy val root : Node = buildBVH(shapes)

  def intersect(r: Ray): Option[Hit] = intersect(r, Float.MaxValue)


  private def intersect(r: Ray, maxDist: Float): Option[Hit] = {
    //no children: intersect all shapes and return closest hit if closer than given dist
    //intersect all bounding boxes of children, keep distances
    //start in increasing order:
    // call recursive and

    if(root.children.isEmpty){
      shapes.flatMap { s =>
        s intersect r
      } match {
        case Nil => None
        case xs => Some(xs.minBy(_.distance))
      }
    }else{
      None
    }

  }

  //http://fileadmin.cs.lth.se/cs/Education/EDAN30/lectures/S2-bvh.pdf

  // closest intersection:
  /*
  LocalRay = Ray, CurrentNode = Root
  • Check LocalRay intersection with Root (world box)
  – No hit => return false
  • For (infinity)
      – If (NOT CurrentNode.isLeaf())
          • Intersection test with both child nodes
              – Both nodes hit => Put the one furthest away on the stack. CurrentNode = closest node
                 » continue
              – Only one node hit => CurrentNode = hit node
                  » continue
              – No Hit: Do nothing (let the stack-popping code below be reached)
      – Else // Is leaf
          • For each primitive in leaf perform intersection testing
              – Intersected => update LocalRay.maxT and store ClosestHit
      – EndIf
      – Pop stack until you find a node with t < LocalRay.maxT => CurrentNode = pop’d
          • Stack is empty? => return ClosestHit (no closest hit => return false, otherwise return true
  • EndFor
*/

//Boolean intersection
/*
• LocalRay = Ray, CurrentNode = Root
• Check LocalRay intersection with Root (world box)
  – No hit => return false
• For (infinity)
    – If (NOT CurrentNode.isLeaf())
        • Intersection test with both child nodes
            – Both nodes hit => Put right one on the stack. CurrentNode = left node
                » Goto LOOP;
            – Only one node hit => CurrentNode = hit node
                » Goto LOOP;
        – No Hit: Do nothing (let the stack-popping code below be reached)
    – Else // Is leaf
        • For each primitive in leaf perform intersection testing
             – Intersected => return true;
    – EndIf
    – Pop stack, CurrentNode = pop’d node
        • Stack is empty => return false
• EndFor

 */


 //build:

  /*

void build(const std::vector<Intersectable *> &objects)
  • Create new vector for Intersectable pointer copies
  • Create new vector for the nodes
  • Create Root node
  • worldBox = AABB(); // world bounding box
  • For each intersectable[i] in objects
      – worldBox.include(intersectable[i] bounding box)
      – Objs.push_back(intersectable[i])
  • EndFor
  • Set world bounding box to root node
  • build_recursive(0, Objs.size(), root, 0);
  The declaration was: void build_recursive(int left_index, int right_index, BVHNode *node, int depth);


  void build_recursive(int left_index, int right_index, BVHNode *node, int depth)
• If ((right_index – left_index) <= Threshold || (other termination criteria))
    – Initiate current node as a leaf with primitives from Objs[left_index] to Objs[right_index]
• Else
    – Split intersectables into left and right by finding a split_index
• Make sure that neither left nor right is completely empty
    – Calculate bounding boxes of left and right sides
    – Create two new nodes, leftNode and rightNode and assign bounding boxes
    – Initiate current node as an interior node with leftNode and rightNode as children
    – build_recursive(left_index, split_index, leftNode, depth + 1)
    – build_recursive(split_index, right_index, rightNode, depth + 1)
• EndIf
   */
}
