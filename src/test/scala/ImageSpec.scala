import color.RGB
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import support.Image

class ImageSpec extends Specification {



  override def is: SpecStructure =
    s2"""
    An image should
      when initialized
        succeed for correct parameters $testInitImage
        fail for a negtive height $testNegativeHeight
        fail for a negtive width $testNegativeWidth
      when setting colors
        succeed for correct indices $testSetColorsForCorrectIndices
        fail for incorrect indices $testSetColorsForIncorrectIndices
      when getting a color
        succeed for valid indices $testGetWithCorrectParameter
        fail for invalid parameter $testGetWithIncorrectParameter
      be saved $testSaveImage
      detect the set of edges $testDetectEdges
      """

  val testInitImage = {
    val img = new Image(400, 600)
    (img.width, img.height) must be equalTo(400, 600)
  }

  val testNegativeHeight = {
    new Image(-2, 10) must throwA[IllegalArgumentException]
  }

  val testNegativeWidth = {
    new Image(200, -121) must throwA[IllegalArgumentException]
  }

  val testSaveImage = {
    val img = new Image(400, 600)
    img.save("blub.image.jpg")
    img.save("blub")
    //TODO: injection for proper test
    1 must beLessThan(2)
  }

  val testSetColorsForCorrectIndices = {
    val img = new Image(400, 600)
    val success = img.set(12, 10, RGB.RED)

    (success must be equalTo true) and
      (img.get(12, 10) must be equalTo Some(RGB.RED))
  }

  val testSetColorsForIncorrectIndices = {
    val img = new Image(400, 600)
    val successNegativeIndex = img.set(-12, 10, RGB.RED)
    val successOutOfBounds = img.set(12, 1000, RGB.RED)

    (successNegativeIndex, successOutOfBounds) must be equalTo (false, false)
  }

  val testGetWithIncorrectParameter = {
    val img = new Image(400, 600)
    val successNegativeIndex = img.get(-12, 10)
    val successOutOfBounds   = img.get(12, 1000)

    (successNegativeIndex, successOutOfBounds) must be equalTo (None, None)
  }

  val testGetWithCorrectParameter = {
    val img = new Image(400, 600)

    val colors : Set[Option[RGB]]= (for{
      x <- 0 until 400
      y <- 0 until 600
    } yield img.get(x,y)).toSet

    colors should be equalTo Set(Some(RGB.BLACK))
  }

  val testDetectEdges = {
    val img = new Image(400, 600)

    Set( (4,4), (4,5), (4,6),
         (5,4),        (5,6),
         (6,4), (6,5), (6,6)) foreach { case (x,y) => img.set(x,y,RGB.WHITE)}


    val expectedEdges  = Set( (3,3), (3,4), (3,5), (3,6), (3,7),
                              (4,3), (4,4), (4,5), (4,6), (4,7),
                              (5,3), (5,4), /*--*/ (5,6), (5,7),
                              (6,3), (6,4), (6,5), (6,6), (6,7),
                              (7,3), (7,4), (7,5), (7,6), (7,7))

    val edges = img.detectEdges()
    edges.toList.sorted should be equalTo expectedEdges.toList.sorted
  }


}