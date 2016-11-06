
import material.Material
import math.Ray
import org.specs2.{ScalaCheck, Specification}
import play.api.libs.json.Json

class MaterialSpec  extends Specification with ScalaCheck {def is = s2"""
       A SingleColorMaterial should
          be parsed from Json ${parseSingleColorMaterial}
    """

  def parseSingleColorMaterial = {

    val in: Material = Material.DEFAULT_MATERIAL
    val js  = Json.toJson(in)
    val out = Json.fromJson[Material](js).get
    out should be equalTo in
  }

}


//see: http://stackoverflow.com/questions/17021847/noise-free-json-format-for-sealed-traits-with-play-2-2-library