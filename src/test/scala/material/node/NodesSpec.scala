package material.node

import org.specs2.ScalaCheck
import generators.nodes.ValueNodeGenerators._
import generators.nodes.VectorNodeGenerators._
import generators.nodes.ColorNodeGenerators._
import org.scalacheck.Prop._
import org.specs2.Specification
import io.circe.parser.decode
import io.circe.syntax._
import org.scalacheck.{Gen, Prop}

class NodesSpec extends Specification with ScalaCheck {
  def is = s2"""
   A ValueNode should encode and decode from type
      CheckerValue          ${testValueNode(checkerValue)}
      ConstantValue         ${testValueNode(constantValue)}
      NoiseValue            ${testValueNode(noiseValue)}
      MultilayerNoiseValue  ${testValueNode(multilayerNoiseValue)}
      ClampValue            ${testValueNode(clampValue)}
      ModValue              ${testValueNode(modValue)}
      AddValues             ${testValueNode(addValues)}
      SubtractValues        ${testValueNode(subtractValues)}
      MultiplyValues        ${testValueNode(multiplyValues)}
      MixValues             ${testValueNode(mixValues)}

   A VectorNode should encode and decode from type
      ConstantVector        ${testVectorNode(constantVector)}
      ColorToVector         ${testVectorNode(colorToVector)}
      ValuesToVector        ${testVectorNode(valuesToVector)}
      SingleValueToVector   ${testVectorNode(singleValueToVector)}
      ClampVector           ${testVectorNode(clampVector)}
      ModVector             ${testVectorNode(modVector)}
      AddVector             ${testVectorNode(addVector)}
      SubtractVector        ${testVectorNode(subtractVector)}
      MultiplyVector        ${testVectorNode(multiplyVector)}
      MixVector             ${testVectorNode(mixVector)}

   A ColorNode should encode and decode from type
      ConstantColor         ${testColorNode(constantColor)}
      CheckerColor          ${testColorNode(checkerColor)}
      PointFallColor        ${testColorNode(pointFallColor)}
      VectorToColor         ${testColorNode(vectorToColor)}
      ValuesToColor         ${testColorNode(valuesToColor)}
      SingleValueToColor    ${testColorNode(singleValueToColor)}
      ClampColor            ${testColorNode(clampColor)}
      ModColor              ${testColorNode(modColor)}
      AddColor              ${testColorNode(addColor)}
      SubtractColor         ${testColorNode(subtractColor)}
      MultiplyColor         ${testColorNode(multiplyColor)}
      MixColor              ${testColorNode(mixColor)}
    """

  def testValueNode(gen: Gen[ValueNode]): Prop = forAll(gen) { node: ValueNode =>
    decode[ValueNode](node.asJson.toString) should beRight(node)
  }

  def testVectorNode(gen: Gen[VectorNode]): Prop = forAll(gen) { node: VectorNode =>
    decode[VectorNode](node.asJson.toString) should beRight(node)
  }

  def testColorNode(gen: Gen[ColorNode]): Prop = forAll(gen){ node: ColorNode =>
    decode[ColorNode](node.asJson.toString) should beRight(node)
  }
}
