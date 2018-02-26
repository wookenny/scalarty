package material.node
import color.RGB
import math.Vector3
import Math.exp
import io.circe._, io.circe.generic.semiauto._

//TODO: color nodes should be limited to 0 to 1 as values!!!


case class ConstantColor(color: RGB) extends ColorNode {
  override def value(position: Vector3): RGB = color.clamp
}

case class CheckerColor(color1: RGB,
                        color2: RGB,
                        stepSize: Double,
                        offset: Option[Vector3] = None,
                        scaling: Option[Vector3] = None) extends ColorNode {
  private val checkerPattern = CheckerValue(stepSize,offset,scaling)
  override def value(position: Vector3): RGB = if(checkerPattern.value(position) > 0.5) color1.clamp
                                               else color2.clamp
}

//TODO falloff between two points
case class PointFallColor(color1: RGB,
                          color2: RGB,
                          center: Vector3,
                          scaling: Option[Double] = Some(0.1)
                         ) extends ColorNode {
  override def value(position: Vector3): RGB = {
    val mix = 1/exp(scaling.getOrElse(0.1) * (center - position).length)
    (color1*mix + color2*(1-mix)).clamp
  }
}

//from other type nodes
case class VectorToColor(node: VectorNode) extends ColorNode {
  override def value(position: Vector3): RGB = {
    val vector = node.value(position)
    RGB(vector.x,vector.y,vector.z).clamp
  }
}

case class ValuesToColor(node1: ValueNode,node2: ValueNode,node3: ValueNode) extends ColorNode {
  override def value(position: Vector3): RGB =
    RGB(node1.value(position), node2.value(position), node3.value(position)).clamp
}

case class SingleValueToColor(node: ValueNode) extends ColorNode {
  override def value(position: Vector3): RGB = {
    val value = node.value(position)
    RGB(value, value, value).clamp
  }
}

// map single value
case class ClampColor(node: ColorNode,
                       minValue: Double,
                       maxValue: Double) extends ColorNode{
  override def value(position: Vector3): RGB = node.value(position).map(minValue max _ min maxValue).clamp
}

case class ModColor(node: ColorNode,
                     mod: Double) extends ColorNode{
  override def value(position: Vector3): RGB = node.value(position).map(_ % mod).clamp
}


// mix nodes
case class AddColor(node1: ColorNode,
                     node2: ColorNode) extends ColorNode{
  override def value(position: Vector3): RGB = (node1.value(position) + node2.value(position)).clamp
}

case class SubtractColor(node1: ColorNode,
                          node2: ColorNode) extends ColorNode{
  override def value(position: Vector3): RGB = (node1.value(position) - node2.value(position)).clamp
}

case class MultiplyColor(node1: ColorNode,
                         node2: ColorNode) extends ColorNode{
  override def value(position: Vector3): RGB = (node1.value(position) mult node2.value(position)).clamp
}

case class MixColor(node1: ColorNode,
                     node2: ColorNode,
                     mixValue: ValueNode) extends ColorNode{
  override def value(position: Vector3): RGB =  {
    val mix = 0d max mixValue.value(position) min 1d
    (node1.value(position)*mix + node2.value(position)*(1-mix)).clamp
  }
}


