package noise

object Gradient {

  val EPS = 0.01

  def gradient(x: Double, y: Double)(implicit noise:Noise): (Double,Double) =
    ((noise.value(x+EPS,y) - noise.value(x-EPS,y)) / (2*EPS) ,
     (noise.value(x,y+EPS) - noise.value(x,y-EPS)) / (2*EPS))

  def gradient(x: Double, y: Double, z:Double)(implicit noise:Noise): (Double,Double,Double) =
    ((noise.value(x+EPS,y,z) - noise.value(x-EPS,y,z)) / (2*EPS),
      (noise.value(x,y+EPS,z) - noise.value(x,y-EPS,z)) / (2*EPS),
      (noise.value(x,y,z+EPS) - noise.value(x,y,z-EPS)) / (2*EPS)
    )

  def gradient(x: Double, y: Double, z:Double, w: Double)(implicit noise:Noise):
  (Double,Double,Double, Double) =
    ((noise.value(x+EPS,y,z,w) - noise.value(x-EPS,y,z,w)) / (2*EPS),
      (noise.value(x,y+EPS,z,w) - noise.value(x,y-EPS,z,w)) / (2*EPS),
      (noise.value(x,y,z+EPS,w) - noise.value(x,y,z-EPS,w)) / (2*EPS),
      (noise.value(x,y,z,w+EPS) - noise.value(x,y,z,w-EPS)) / (2*EPS)
    )

}
