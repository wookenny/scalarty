
import shapeless.{Id, Poly, Poly0, Tuple}
import shapeless.syntax.std.tuple._

import scala.util.Random
import shapeless.poly._

val r1 = Random.nextDouble()
val r2 = Random.nextDouble()
/*
val (x,y,z,w)= Tuple.fill(4)(1).map(x:Int => Random.nextDouble())

*/

Tuple.fill(4)(Random.nextDouble())

List.tabulate(4)(_ => Random.nextDouble)


//(1,2,3,4,5,6,7,8).map{case _:Int =>  Random.nextDouble()}

object option extends (Id ~> Option) {
  def apply[T](t: T) = Option(t)
}

/*
object initR extends (Int ~> Double) {
  def apply[Double](x: Int) = Random.nextDouble()
}
*/

(1,1,1) map option


(1,1,1) map plusOne