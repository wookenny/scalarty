package noise

import math.{FourDimensionalFunction, ThreeDimensionalFunction, TwoDimensionalFunction}

trait Noise extends TwoDimensionalFunction[Double]
  with ThreeDimensionalFunction[Double]
  with FourDimensionalFunction[Double]{
  val seed: Long
  val noiseSize: Double

  override def eval(x: Double, y: Double) = valueFunction(x/noiseSize, y/noiseSize)
  override def eval(x: Double, y: Double, z: Double) = valueFunction(x/noiseSize, y/noiseSize, z/noiseSize)
  override def eval(x: Double, y: Double, z: Double, w: Double) = valueFunction(x/noiseSize, y/noiseSize, z/noiseSize, w/noiseSize)


  def evalNormalized(x: Double, y: Double) = normalizeValue2(eval(x,y))
  def evalNormalized(x: Double, y: Double, z: Double) = normalizeValue3(eval(x,y,z))
  def evalNormalized(x: Double, y: Double, z: Double, w: Double) = normalizeValue4(eval(x,y,z,w))

  private[noise] def valueFunction(x: Double, y: Double) : Double
  private[noise] def valueFunction(x: Double, y: Double, z: Double): Double
  private[noise] def valueFunction(x: Double, y: Double, z: Double, w: Double): Double


  def range2 : (Double,Double)
  def range3 : (Double,Double)
  def range4 : (Double,Double)


  private def normalizeValue2(v:Double)   = (v - range2._1) /  (range2._2 - range2._1)
  private def normalizeValue3(v:Double)   = (v - range3._1) /  (range3._2 - range3._1)
  private def normalizeValue4(v:Double)   = (v - range4._1) /  (range4._2 - range4._1)
}
