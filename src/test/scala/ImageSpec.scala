import java.awt.Color

import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import support.Image

class ImageSpec extends Specification {

  override def is: SpecStructure = s2"""
    An image should
      be initialized correctly $testInitImage
      be saced $testSaveImage
      set colors correctly $testSetColors
      be invalid with negtive height $testNegativeHeight
      be invalid with negtive widtht $testNegativeWidth
    """



  val testInitImage = {
    val img = new Image(400,600)
    //TODO: injection for proper test
    1 must beLessThan(2)
  }

  val testSaveImage = {
    val img = new Image(400,600)
    img.save("blub.image.jpg")
    img.save("blub")
    //TODO: injection for proper test
    1 must beLessThan(2)
  }

  val testSetColors = {
    val img = new Image(400,600)
    img.set(12,10, Color.RED)
    //TODO: injection for proper test
    1 must beLessThan(2)
  }

  val testNegativeHeight = {
    (new Image(-2,10)) must throwA[IllegalArgumentException]
  }

  val testNegativeWidth = {
    (new Image(200,-121)) must throwA[IllegalArgumentException]
  }

}
