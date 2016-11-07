package color

import play.api.libs.json.{Json, OFormat}

final case class RGB(red: Float, green: Float, blue: Float) {
  def ^(pow: Float) = RGB(
    Math.pow(red, pow).toFloat,
    Math.pow(green, pow).toFloat,
    Math.pow(blue, pow).toFloat
  )

  def awtColor() = new java.awt.Color(
    Math.min(Math.max(0f, red), 1f),
    Math.min(Math.max(0f, green), 1f),
    Math.min(Math.max(0f, blue), 1f)
  )

  def *(s: Float) = RGB(red * s, green * s, blue * s)
  def /(s: Float) = RGB(red / s, green / s, blue / s)

  def +(c: RGB) = RGB(red + c.red, green + c.green, blue + c.blue)
  def -(c: RGB) = RGB(red - c.red, green - c.green, blue - c.blue)

  def unary_-() = this * (-1)
  def unary_+() = this

  def exposureCorrected = RGB(
    1 - Math.exp(-red).toFloat,
    1 - Math.exp(-green).toFloat,
    1 - Math.exp(-blue).toFloat
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

  val GAMMA = 2.2f

  def apply(r: Double, g: Double, b: Double): RGB = apply(r.toFloat, g.toFloat, b.toFloat)
}
