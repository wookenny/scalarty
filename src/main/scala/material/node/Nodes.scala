package material.node

import color.RGB
import math.Vector3
import math.ThreeDimensionalFunction
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._
import cats.syntax.functor._


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

  implicit val encodeValueNode: Encoder[ValueNode] = Encoder.instance {
    case n @ ConstantValue(_)     => Json.obj("ConstantValue"    -> n.asJson)
    case n @ CheckerValue(_,_,_,_,_)     => Json.obj("CheckerValue"    -> n.asJson)
    case n @ NoiseValue(_,_)     => Json.obj("NoiseValue"    -> n.asJson)
    case n @ MultilayerNoiseValue(_,_,_)     => Json.obj("MultilayerNoiseValue"    -> n.asJson)
    case n @ ClampValue(_,_,_)     => Json.obj("ClampValue"    -> n.asJson)
    case n @ AddValues(_,_)     => Json.obj("AddValues"    -> n.asJson)
    case n @ SubtractValues(_,_)     => Json.obj("SubtractValues"    -> n.asJson)
    case n @ MultiplyValues(_,_)     => Json.obj("MultiplyValues"    -> n.asJson)
    case n @ MixValues(_,_,_)     => Json.obj("MixValues"    -> n.asJson)
  }

  private val decodeConstantValue = Decoder[ConstantValue].prepare(_.downField("ConstantValue"))
  private val decodeCheckerValue = Decoder[CheckerValue].prepare(_.downField("CheckerValue"))
  private val decodNoiseValue = Decoder[NoiseValue].prepare(_.downField("NoiseValue"))
  private val decodeMultilayerNoiseValue = Decoder[MultilayerNoiseValue].prepare(_.downField("MultilayerNoiseValue"))
  private val decodeClampValue = Decoder[ClampValue].prepare(_.downField("ClampValue"))
  private val decodeAddValues = Decoder[AddValues].prepare(_.downField("AddValues"))
  private val decodeSubtractValues = Decoder[SubtractValues].prepare(_.downField("SubtractValues"))
  private val decodeMultiplyValues = Decoder[MultiplyValues].prepare(_.downField("MultiplyValues"))
  private val decodeMixValues = Decoder[MixValues].prepare(_.downField("MixValues"))

  implicit val decodeValueNode: Decoder[ValueNode] = decodeConstantValue
    .or(decodeCheckerValue.widen[ValueNode])
    .or(decodNoiseValue.widen[ValueNode])
    .or(decodeMultilayerNoiseValue.widen[ValueNode])
    .or(decodeClampValue.widen[ValueNode])
    .or(decodeAddValues.widen[ValueNode])
    .or(decodeSubtractValues.widen[ValueNode])
    .or(decodeMultiplyValues.widen[ValueNode])
    .or(decodeMixValues.widen[ValueNode])

}

object VectorNode{

  implicit val encodeVectorNode: Encoder[VectorNode] = Encoder.instance {
    case n @ ConstantVector(_)     => Json.obj("ConstantVector"    -> n.asJson)
    case n @ ColorToVector(_)     => Json.obj("ColorToVector"    -> n.asJson)
    case n @ ValuesToVector(_,_,_)     => Json.obj("ValuesToVector"    -> n.asJson)
    case n @ SingleValueToVector(_)     => Json.obj("SingleValueToVector"    -> n.asJson)
    case n @ ClampVector(_,_,_)     => Json.obj("ClampVector"    -> n.asJson)
    case n @ ModVector(_,_)     => Json.obj("ModVector"    -> n.asJson)
    case n @ AddVector(_,_)     => Json.obj("AddVector"    -> n.asJson)
    case n @ SubtractVector(_,_)     => Json.obj("SubtractVector"    -> n.asJson)
    case n @ MultiplyVector(_,_)     => Json.obj("MultiplyVector"    -> n.asJson)
    case n @ MixVector(_,_,_)     => Json.obj("MixVector"    -> n.asJson)
  }

  private val decodeConstantVector = Decoder[ConstantVector].prepare(_.downField("ConstantVector"))
  private val decodeColorToVector = Decoder[ColorToVector].prepare(_.downField("ColorToVector"))
  private val decodeValuesToVector = Decoder[ValuesToVector].prepare(_.downField("ValuesToVector"))
  private val decodeSingleValueToVector = Decoder[SingleValueToVector].prepare(_.downField("SingleValueToVector"))
  private val decodeClampVector = Decoder[ClampVector].prepare(_.downField("ConstantVector"))
  private val decodeModVector = Decoder[ModVector].prepare(_.downField("ModVector"))
  private val decodeAddVector = Decoder[AddVector].prepare(_.downField("AddVector"))
  private val decodeSubtractVector = Decoder[SubtractVector].prepare(_.downField("SubtractVector"))
  private val decodeMultiplyVector = Decoder[MultiplyVector].prepare(_.downField("MultiplyVector"))
  private val decodeMixVector = Decoder[MixVector].prepare(_.downField("MixVector"))

  implicit val decodeVectorNode: Decoder[VectorNode] = decodeConstantVector
    .or(decodeColorToVector.widen[VectorNode])
    .or(decodeValuesToVector.widen[VectorNode])
    .or(decodeSingleValueToVector.widen[VectorNode])
    .or(decodeClampVector.widen[VectorNode])
    .or(decodeModVector.widen[VectorNode])
    .or(decodeAddVector.widen[VectorNode])
    .or(decodeSubtractVector.widen[VectorNode])
    .or(decodeMultiplyVector.widen[VectorNode])
    .or(decodeMixVector.widen[VectorNode])

}

object ColorNode{

  implicit val encodeColorNode: Encoder[ColorNode] = Encoder.instance {
    case n @ ConstantColor(_) => Json.obj("ConstantColor" -> n.asJson)
    case n @ CheckerColor(_, _, _, _, _) => Json.obj("CheckerColor" -> n.asJson)
    case n @ PointFallColor(_, _, _, _) => Json.obj("PointFallColor" -> n.asJson)
    case n @ VectorToColor(_) => Json.obj("VectorToColor" -> n.asJson)
    case n @ ValuesToColor(_, _, _) => Json.obj("ValuesToColor" -> n.asJson)
    case n @ SingleValueToColor(_) => Json.obj("SingleValueToColor" -> n.asJson)
    case n @ ClampColor(_, _, _) => Json.obj("ClampColor" -> n.asJson)
    case n @ ModColor(_, _) => Json.obj("ModColor" -> n.asJson)
    case n @ AddColor(_, _) => Json.obj("AddColor" -> n.asJson)
    case n @ SubtractColor(_, _) => Json.obj("SubtractColor" -> n.asJson)
    case n @ MultiplyColor(_, _) => Json.obj("MultiplyColor" -> n.asJson)
    case n @ MixColor(_, _, _) => Json.obj("MixColor" -> n.asJson)
  }

  private val decodeConstantColor = Decoder[ConstantColor].prepare(_.downField("ConstantColor"))
  private val decodeCheckerColor = Decoder[CheckerColor].prepare(_.downField("CheckerColor"))
  private val decodePointFallColor = Decoder[PointFallColor].prepare(_.downField("PointFallColor"))
  private val decodeVectorToColor = Decoder[VectorToColor].prepare(_.downField("VectorToColor"))
  private val decodeValuesToColor = Decoder[ValuesToColor].prepare(_.downField("ValuesToColor"))
  private val decodeSingleValueToColor = Decoder[SingleValueToColor].prepare(_.downField("SingleValueToColor"))
  private val decodeClampColor = Decoder[ClampColor].prepare(_.downField("ClampColor"))
  private val decodeModColor = Decoder[ModColor].prepare(_.downField("ModColor"))
  private val decodeAddColor = Decoder[AddColor].prepare(_.downField("AddColor"))
  private val decodeSubtractColor = Decoder[SubtractColor].prepare(_.downField("SubtractColor"))
  private val decodeMultiplyColor = Decoder[MultiplyColor].prepare(_.downField("MultiplyColor"))
  private val decodeMixColor = Decoder[MixColor].prepare(_.downField("MixColor"))

  implicit val decodeColorNode: Decoder[ColorNode] = decodeConstantColor
    .or(decodeCheckerColor.widen[ColorNode])
    .or(decodePointFallColor.widen[ColorNode])
    .or(decodeVectorToColor.widen[ColorNode])
    .or(decodeValuesToColor.widen[ColorNode])
    .or(decodeSingleValueToColor.widen[ColorNode])
    .or(decodeClampColor.widen[ColorNode])
    .or(decodeModColor.widen[ColorNode])
    .or(decodeAddColor.widen[ColorNode])
    .or(decodeSubtractColor.widen[ColorNode])
    .or(decodeMultiplyColor.widen[ColorNode])
    .or(decodeMixColor.widen[ColorNode])
}


