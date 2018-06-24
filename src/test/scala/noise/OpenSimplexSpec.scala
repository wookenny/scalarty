package noise

import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class OpenSimplexSpec extends Specification with ScalaCheck {
  override def is: SpecStructure =
    s2"""
      An OpenSimplx should
        different value for different seed

    """

  val simplex: OpenSimplex = OpenSimplex(2, noiseSize = 2)

  //2/3/4d:


  // simplex.valueFunction(1,1) // different value for different seed,
  // value in range specified
}
