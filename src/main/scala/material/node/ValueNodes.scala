package material.node
import math.Vector3
import noise._
import Math.abs

case class ConstantValue(value: Double) extends ValueNode {
  override def value(vector: Vector3): Double = value
}

case class CheckerValue(stepSize: Double,
                        offset: Option[Vector3] = None,
                        scaling: Option[Vector3] = None,
                        value1: Option[Double] = None,
                        value2: Option[Double] = None)
    extends ValueNode {

  private def mod(x: Double, m: Double) = (x % m + abs(m)) % m
  private def inStep(position: Double): Boolean = mod(position, 2 * stepSize) >= stepSize

  override def value(position: Vector3): Double = {
    val pos: Vector3 = (position - offset.getOrElse(Vector3.ZERO)) mult scaling.getOrElse(
      Vector3.ONE)
    if (inStep(pos.x) ^ inStep(pos.y) ^ inStep(pos.z))
      value1.getOrElse(1d)
    else
      value2.getOrElse(0d)
  }
}

case class NoiseValue(seed: Long, size: Double = 1) extends ValueNode {

  val noise = new OpenSimplex(seed, size)

  override def value(position: Vector3): Double =
    noise.evalNormalized(position.x, position.y, position.z)
}

case class MultilayerNoiseValue(seed: Long, size: Double = 1, octaves: Option[Int] = None)
    extends ValueNode {

  val numOctaves = (1 max octaves.getOrElse(7)) max 10

  val noise = new MultilayerNoise(seed, size, numOctaves)

  override def value(position: Vector3): Double =
    noise.evalNormalized(position.x, position.y, position.z)
}

// map single value
case class ClampValue(node: ValueNode, minValue: Double, maxValue: Double) extends ValueNode {
  override def value(position: Vector3): Double = minValue max node.value(position) min maxValue
}

case class ModValue(node: ValueNode, mod: Double) extends ValueNode {
  override def value(position: Vector3): Double = node.value(position) % mod
}

// combine two value nodes
case class AddValues(node1: ValueNode, node2: ValueNode) extends ValueNode {
  override def value(position: Vector3): Double = node1.value(position) + node2.value(position)
}

case class SubtractValues(node1: ValueNode, node2: ValueNode) extends ValueNode {
  override def value(position: Vector3): Double = node1.value(position) - node2.value(position)
}

case class MultiplyValues(node1: ValueNode, node2: ValueNode) extends ValueNode {
  override def value(position: Vector3): Double = node1.value(position) * node2.value(position)
}

case class MixValues(node1: ValueNode, node2: ValueNode, mixValue: ValueNode) extends ValueNode {
  override def value(position: Vector3): Double = {
    val mix = 0d max mixValue.value(position) min 1d
    mix * node1.value(position) + (1 - mix) * node2.value(position)
  }
}
