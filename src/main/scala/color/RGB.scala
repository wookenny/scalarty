package color


final case class RGB(red: Double, green: Double, blue: Double){
  def ^(pow: Double) = RGB(
    Math.pow(red, pow),
    Math.pow(green, pow),
    Math.pow(blue, pow)
  )

  def awtColor() = new java.awt.Color(
    (0f max red.toFloat) min 1f,
    (0f max green.toFloat) min 1f,
    (0f max blue.toFloat) min 1f
  )

  def *(s: Double) = RGB(red * s, green * s, blue * s)
  def /(s: Double) = RGB(red / s, green / s, blue / s)

  def +(c: RGB) = RGB(red + c.red, green + c.green, blue + c.blue)
  def -(c: RGB) = RGB(red - c.red, green - c.green, blue - c.blue)

  def mult(c: RGB) = RGB(red * c.red, green * c.green, blue * c.blue)

  def unary_-(): RGB = this * -1
  def unary_+(): RGB = this

  def map(f: Double => Double) = RGB(f(red),f(green),f(blue))

  def exposureCorrected = RGB(
    1 - Math.exp(-red),
    1 - Math.exp(-green),
    1 - Math.exp(-blue)
  )
  def gammaCorrected: RGB = this ^ (1 / RGB.GAMMA)

  lazy val clamp = this.map(0d max _ min 1d)
}

object RGB {
  val BLACK = RGB(0, 0, 0)
  val WHITE = RGB(1, 1, 1)
  val RED = BLACK.copy(red = 1)
  val GREEN = BLACK.copy(green = 1)
  val BLUE = BLACK.copy(blue = 1)
  val CYAN = BLACK.copy(green = 1, blue = 1)
  val YELLOW = RGB(1, 1, 0)
  val GAMMA = 2.2
}
