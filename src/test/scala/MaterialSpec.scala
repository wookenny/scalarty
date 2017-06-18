import color.RGB
import material.{CheckerMaterial, Material, SingleColorMaterial, UnshadedColor}
import math.{Shape, Vector3}
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop._
import org.specs2.{ScalaCheck, Specification}
import play.api.libs.json.Json

class MaterialSpec extends Specification with ScalaCheck {
  def is =
    s2"""
       A SingleColorMaterial should
          be parsed from Json $parseSingleColorMaterial
          have a single color $testSingleColorMaterial

       A CheckerMaterial should
          be parsed parsed from Json $parseCheckerMaterial
          have a repeating pattern $testCheckerMaterial
       The default material
          should be parsed correctly $parseDefaultMaterial

    """

  implicit lazy val VectorGen: Arbitrary[Vector3] =
    Arbitrary {
      for {
        x: Double <- Gen.choose(-1000d, 1000d)
        y: Double <- Gen.choose(-1000d, 1000d)
        z: Double <- Gen.choose(-1000d, 1000d)
      } yield Vector3(x, y, z)
    }

  //TODO make an own matcher with type T out of it
  private def parseMaterial(in: Material) = {
    val js = Json.toJson(in)
    val out = Json.fromJson[Material](js).get
    out should be equalTo in
  }

  val parseSingleColorMaterial = parseMaterial(
    SingleColorMaterial("noname", RGB.RED, 0.5, 0.1, 0.3, 0.1))
  val parseDefaultMaterial = parseMaterial(Material.DEFAULT_MATERIAL)
  val parseCheckerMaterial = parseMaterial(
    CheckerMaterial("CheckerMaterial1", RGB.WHITE, RGB.BLACK, 0.1, 0.5, 0.3, 0.2))

  val testSingleColorMaterial: Prop = forAll { (x: Vector3) =>
    val mat = SingleColorMaterial("TestMat", RGB.BLUE, 0.4, 0.2, 0.1, 0.1, 0.2)
    val expectedColor = UnshadedColor(RGB.BLUE,
                                      mat.ambient,
                                      mat.diffuse,
                                      mat.spec,
                                      mat.reflective,
                                      mat.refractive,
                                      mat.n,
                                      mat.shininess)

    mat.getMat(x) should be equalTo expectedColor
  }

  val testCheckerMaterial: Prop =
    forAll(Gen.choose(-1000, 1000), Gen.choose(-1000, 1000), Gen.choose(-1000, 1000)) {
      (x, y, z) =>
        {
          val pos = Vector3(x + 0.5, y + 0.5, z + 0.5)
          val mat = CheckerMaterial("CheckerMaterial1", RGB.WHITE, RGB.BLACK, 1, 0.5, 0.3, 0.2)
          val expectedColor =
            if ((x % 2 + y % 2 + z % 2 + 10) % 2 == 1) RGB.WHITE else RGB.BLACK

          mat.getMat(pos).color shouldEqual expectedColor
        }
    }
}
//see: http://stackoverflow.com/questions/17021847/noise-free-json-format-for-sealed-traits-with-play-2-2-library
