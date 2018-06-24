package noise

class MultilayerNoise(
    override val seed: Long,
    override val noiseSize: Double,
    octaves: Int = 7,
    normalized: Boolean = false, //map to [0,1]
    noiseOption: Option[Noise] = None
) extends Noise {

  lazy val noise = noiseOption.getOrElse(new OpenSimplex(seed, noiseSize * octaves))

  private[noise] def valueFunction(x: Double, y: Double): Double = {
    require(octaves >= 0, "number of octaves has to be non-negative")
    val wavelengths = (0 until octaves).map(Math.pow(2, _))
    val scalingFactor = 1d / wavelengths.map(1 / _).sum

    wavelengths.map(l => noise.eval(l * x, l * y) / l).sum * scalingFactor
  }

  private[noise] def valueFunction(x: Double, y: Double, z: Double): Double = {
    require(octaves >= 0, "number of octaves has to be non-negative")
    val wavelengths = (0 until octaves).map(Math.pow(2, _))
    val scalingFactor = 1d / wavelengths.map(1 / _).sum

    wavelengths.map(l => noise.eval(l * x, l * y, l * z) / l).sum * scalingFactor
  }

  private[noise] def valueFunction(x: Double, y: Double, z: Double, w: Double) = {
    require(octaves >= 0, "number of octaves has to be non-negative")
    val wavelengths = (0 until octaves).map(Math.pow(2, _))
    val scalingFactor = 1d / wavelengths.map(1 / _).sum

    wavelengths.map(l => noise.eval(l * x, l * y, l * z, l * w) / l).sum * scalingFactor
  }

  override lazy val range2: (Double, Double) = noise.range2

  override lazy val range3: (Double, Double) = noise.range3

  override lazy val range4: (Double, Double) = noise.range4
}
