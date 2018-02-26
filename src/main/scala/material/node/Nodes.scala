package material.node

import color.RGB
import math.Vector3
import math.ThreeDimensionalFunction
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._



trait ValueNode extends ThreeDimensionalFunction[Double]{
  def value(position: Vector3): Double
  override def eval(x:Double, y:Double, z:Double) = value(Vector3(x,y,z))
}

trait VectorNode extends ThreeDimensionalFunction[Vector3]{
  def value(position: Vector3): Vector3
  override def eval(x:Double, y:Double, z:Double) = value(Vector3(x,y,z))
}

trait ColorNode extends ThreeDimensionalFunction[RGB]{
  def value(position: Vector3): RGB
  override def eval(x:Double, y:Double, z:Double) = value(Vector3(x,y,z))
}


object ValueNode{
  def unapply(node: ValueNode): Option[(String, Json)] = {
    val (prod: Product, sub) = node match {
      case n: ConstantValue => (n, n.asJson)
      case n: CheckerValue => (n, n.asJson)
      case n: NoiseValue => (n, n.asJson)
      case n: MultilayerNoiseValue => (n, n.asJson)
      case n: ClampValue => (n, n.asJson)
      case n: AddValues => (n, n.asJson)
      case n: SubtractValues => (n, n.asJson)
      case n: MultiplyValues => (n, n.asJson)
      case n: MixValues => (n, n.asJson)
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: String): ValueNode = {
    (`type` match {
      case "ConstantValue" => decode[ConstantValue](data)
      case "CheckerValue" => decode[CheckerValue](data)
      case "NoiseValue" => decode[NoiseValue](data)
      case "MultilayerNoiseValue" => decode[MultilayerNoiseValue](data)
      case "ClampValue" => decode[ClampValue](data)
      case "AddValues" => decode[AddValues](data)
      case "SubtractValues" => decode[SubtractValues](data)
      case "MultiplyValues" => decode[MultiplyValues](data)
      case "MixValues" => decode[MixValues](data)
      case _ => Left(Error/*(s"Unknown ValueNode type: ${`type`}")*/)
    }) match {
      case Right(node) => node
      case Left(error) =>  throw new IllegalArgumentException(s"Could parse the Json as ValueNode: $data. Error: $error")
    }
  }

}

object VectorNode{
  
  def unapply(node: VectorNode): Option[(String, Json)] = {
    val (prod: Product, sub) = node match {
      case n: ConstantVector => (n, n.asJson)
      case n: ColorToVector => (n, n.asJson)
      case n: ValuesToVector => (n, n.asJson)
      case n: SingleValueToVector => (n, n.asJson)
      case n: ClampVector => (n, n.asJson)
      case n: ModVector => (n, n.asJson)
      case n: AddVector => (n, n.asJson)
      case n: SubtractVector => (n, n.asJson)
      case n: MultiplyVector => (n, n.asJson)
      case n: MixVector => (n, n.asJson)
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: String): VectorNode = {
    (`type` match {
      case "ConstantVector" => decode[ConstantVector](data)

      case "ColorToVector" => decode[ConstantVector](data)
      case "ValuesToVector" => decode[ValuesToVector](data)
      case "SingleValueToVector" => decode[SingleValueToVector](data)
      case "ClampVector" => decode[ClampVector](data)
      case "ModVector" => decode[ModVector](data)
      case "AddVector" => decode[AddVector](data)
      case "SubtractVector" => decode[SubtractVector](data)
      case "MultiplyVector" => decode[MultiplyVector](data)
      case "MixVector" => decode[MixVector](data)

      case _ => Left(Error/*(s"Unknown VectorNode type: ${`type`}")*/)
    }) match {
      case Right(node) => node
      case Left(error) =>
        throw new IllegalArgumentException(s"Could parse the Json as VectorNode: $data. Error: $error")
    }
  }

}

object ColorNode{

  implicit val mixColorDecoder = Decoder[MixColor]

  def unapply(node: ColorNode): Option[(String, Json)] = {
    val (prod: Product, sub) = node match {
      case n: ConstantColor => (n, n.asJson)
      case n: CheckerColor => (n, n.asJson)
      case n: PointFallColor => (n, n.asJson)
      case n: VectorToColor => (n, n.asJson)
      case n: ValuesToColor => (n, n.asJson)
      case n: SingleValueToColor => (n, n.asJson)
      case n: ClampColor => (n, n.asJson)
      case n: ModColor => (n, n.asJson)
      case n: AddColor => (n, n.asJson)
      case n: SubtractColor => (n, n.asJson)
      case n: MultiplyColor => (n, n.asJson)
      case n: MixColor => (n, n.asJson)
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`type`: String, data: String): ColorNode = {
    (`type` match {
      case "ConstantColor" => decode[ConstantColor](data)
      case "CheckerColor" => decode[CheckerColor](data)
      case "PointFallColor" => decode[PointFallColor](data)
      case "VectorToColor" => decode[VectorToColor](data)
      case "ValuesToColor" => decode[ValuesToColor](data)
      case "SingleValueToColor" => decode[SingleValueToColor](data)
      case "ClampColor" => decode[ClampColor](data)
      case "ModColor" => decode[ModColor](data)
      case "AddColor" => decode[AddColor](data)
      case "SubtractColor" => decode[SubtractColor](data)
      case "MultiplyColor" => decode[MultiplyColor](data)
      case "MixColor" => decode[MixColor](data)

      case _ => Left(Error/*(s"Unknown ColorNode type: ${`type`}")*/)
    }) match {
      case Right(node) => node
      case Left(error) =>
        throw new IllegalArgumentException(s"Could parse the Json as ColorNode: $data. Error: $error")
    }
  }


}


