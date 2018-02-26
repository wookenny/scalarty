package material.node

import math.Vector3

case class ConstantVector(vector: Vector3) extends VectorNode {
  override def value(position: Vector3): Vector3 = vector
}

//from other type nodes
case class ColorToVector(node: ColorNode) extends VectorNode {
  override def value(position: Vector3): Vector3 = {
    val rgb = node.value(position)
    Vector3(rgb.red, rgb.green, rgb.green)
  }
}

case class ValuesToVector(node1: ValueNode,node2: ValueNode,node3: ValueNode) extends VectorNode {
  override def value(position: Vector3): Vector3 =
    Vector3(node1.value(position), node2.value(position), node3.value(position))
}

case class SingleValueToVector(node: ValueNode) extends VectorNode {
  override def value(position: Vector3): Vector3 = {
    val value = node.value(position)
    Vector3(value, value, value)
  }
}

// map single value
case class ClampVector(node: VectorNode,
                      minValue: Double,
                      maxValue: Double) extends VectorNode{
  override def value(position: Vector3): Vector3 = node.value(position).map(minValue max _ min maxValue)
}

case class ModVector(node: VectorNode,
                     mod: Double) extends VectorNode{
  override def value(position: Vector3): Vector3 = node.value(position).map(_ % mod)
}


// mix nodes
case class AddVector(node1: VectorNode,
                     node2: VectorNode) extends VectorNode{
  override def value(position: Vector3): Vector3 = node1.value(position) + node2.value(position)
}

case class SubtractVector(node1: VectorNode,
                          node2: VectorNode) extends VectorNode{
  override def value(position: Vector3): Vector3 = node1.value(position) - node2.value(position)
}

case class MultiplyVector(node1: VectorNode,
                          node2: VectorNode) extends VectorNode{
  override def value(position: Vector3): Vector3 = node1.value(position) mult node2.value(position)
}

case class MixVector(node1: VectorNode,
                     node2: VectorNode,
                     mixValue: ValueNode) extends VectorNode{
  override def value(position: Vector3): Vector3 =  {
    val mix = 0d max mixValue.value(position) min 1d
    node1.value(position)*mix + node2.value(position)*(1-mix)
  }
}
