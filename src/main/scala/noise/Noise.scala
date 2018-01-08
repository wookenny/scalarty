package noise

trait Noise {
  val seed: Long

  /*
    The noise value is in the range from -½√n to ½√n, wher n is the dimension.
      * 2D noise: [-0.707,0.707]
      * 3D noise: [-0.866,0.866]
      * 4D noise: [-1,1]
  */

  def value(x: Double, y: Double): Double
  def value(x: Double, y: Double, z: Double): Double
  def value(x: Double, y: Double, z: Double, w: Double): Double
}
