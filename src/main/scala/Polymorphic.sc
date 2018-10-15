import cats.syntax.functor._
import io.circe.{Decoder, Encoder, Error, Json}
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._


trait Node{
  def get : Double
}
case class Terminal(a:Double) extends Node {
  override def get: Double = a
}
case class Single(next:Node) extends Node {
  override def get: Double = next.get
}
case class Cross(a:Node, b:Node) extends Node {
  override def get: Double = a.get + b.get
}

object Node{

  implicit val encodeNode: Encoder[Node] = Encoder.instance {
    case n @ Single(_)     => Json.obj("Single"    -> n.asJson)
    case n @ Terminal(_)   => Json.obj("Terminal"  -> n.asJson)
    case n @ Cross(_,_)   => Json.obj("Cross"      -> n.asJson)
  }

  private val decodeTerminal = Decoder[Terminal].prepare(_.downField("Terminal"))
  private val decodeSingle = Decoder[Single].prepare(_.downField("Single"))
  private val decodeCross = Decoder[Cross].prepare(_.downField("Cross"))

  implicit val decodeNode: Decoder[Node] = decodeTerminal.widen[Node]
                                           .or(decodeSingle.widen[Node])
                                           .or(decodeCross.widen[Node])
}


val n1 : Node = Single(Terminal(2f))
val n2 : Node = Terminal(2f)
val n3 : Node = Cross(Single(Terminal(2f)),Cross(Terminal(7f),Terminal(6f)))


n1.asJson
n2.asJson
n3.asJson

val nodes : Seq[Node] = Seq(
  Single(Terminal(3f)),
  Terminal(5.46f),
  Cross(Single(Terminal(2f)),Cross(Terminal(7f),Terminal(6f)))
)

  val seq1: Seq[Either[Error, Node]] = nodes.map(_.asJson).map(x => decode[Node](x.toString))

  val seq2: Seq[Node] = nodes.map(_.asJson).flatMap(x => decode[Node](x.toString).toOption)

seq2.map(_.get)