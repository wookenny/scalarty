package math

trait OneDimensionalFunction[T] {
  def eval(x:Double) : T
}

trait TwoDimensionalFunction[T] {
  def eval(x:Double, y:Double) : T
}

trait ThreeDimensionalFunction[T] {
  def eval(x:Double, y: Double, z: Double) : T
}

trait FourDimensionalFunction[T] {
  def eval(x:Double, y: Double, z: Double, w: Double) : T
}
