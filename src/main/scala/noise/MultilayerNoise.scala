package noise

object MultilayerNoise {


  def multilayerNoise2(x: Double, y: Double, noiseSize: Double, octaves: Int = 7)
                      (implicit noise:Noise): Double = {
    require(octaves >= 0, "number of octaves has to be non-negative")
    val wavelengths = (0 until octaves).map(Math.pow(2,_))
    val scalingFactor = 1d/ wavelengths.map(1/_).sum

    wavelengths.map(l => noise.value(l*x/noiseSize,l*y/noiseSize) / l).sum * scalingFactor
  }

  def multilayerNoise3(x: Double, y: Double, z:Double, noiseSize: Double, octaves: Int = 7)
                      (implicit noise:Noise): Double = {
    require(octaves >= 0, "number of octaves has to be non-negative")
    val wavelengths = (0 until octaves).map(Math.pow(2,_))
    val scalingFactor = 1d/ wavelengths.map(1/_).sum


    wavelengths.map(l => noise.value(l*x/noiseSize,l*y/noiseSize,l*z/noiseSize) / l).sum * scalingFactor
  }

  def multilayerNoise4(x: Double, y: Double, z:Double, w:Double, noiseSize: Double, octaves: Int = 7)
                      (implicit noise:Noise): Double = {
    require(octaves >= 0, "number of octaves has to be non-negative")
    val wavelengths = (0 until octaves).map(Math.pow(2,_))
    val scalingFactor = 1d/ wavelengths.map(1/_).sum

    wavelengths.map(l => noise.value(l*x/noiseSize,l*y/noiseSize,l*z/noiseSize, l*w/noiseSize) / l).sum * scalingFactor
  }

}
