package color

import play.api.libs.json.{Json, OFormat}

final case class RGB(red: Double, green: Double, blue: Double) {
  def ^(pow: Double) = RGB(
    Math.pow(red, pow),
    Math.pow(green, pow),
    Math.pow(blue, pow)
  )

  def awtColor() = new java.awt.Color(
    (0f max red.toFloat)   min 1f,
    (0f max green.toFloat) min 1f,
    (0f max blue.toFloat)  min 1f
  )

  def *(s: Double) = RGB(red * s, green * s, blue * s)
  def /(s: Double) = RGB(red / s, green / s, blue / s)

  def +(c: RGB) = RGB(red + c.red, green + c.green, blue + c.blue)
  def -(c: RGB) = RGB(red - c.red, green - c.green, blue - c.blue)

  def unary_-() = this * (-1)
  def unary_+() = this

  def exposureCorrected = RGB(
    1 - Math.exp(-red),
    1 - Math.exp(-green),
    1 - Math.exp(-blue)
  )
  def gammaCorrected = this ^ (1 / RGB.GAMMA)

}

object RGB {

  implicit val colorJsonFormat: OFormat[RGB] = Json.format[RGB]

  val BLACK = RGB(0, 0, 0)
  val WHITE = RGB(1, 1, 1)
  val RED = BLACK.copy(red = 1)
  val GREEN = BLACK.copy(green = 1)
  val BLUE = BLACK.copy(blue = 1)
  val CYAN = BLACK.copy(green = 1, blue = 1)

  val GAMMA = 2.2

  //def apply(r: Double, g: Double, b: Double): RGB =
  //  apply(r.toDouble, g.toDouble, b.toDouble)
}
