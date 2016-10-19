import java.util.Random

import com.typesafe.scalalogging._
import geometry._


object Renderer{
  val backgroundColor = RGB.BLACK
}

class Renderer(val scene: Scene) extends LazyLogging {

  def shadowRay(position: Vector3, light: Vector3): Boolean = {
      val vectorToLight = (light-position)
      anyHit( Ray(position, vectorToLight.normalized), vectorToLight.length )
  }

  def shadeHit(hit: Hit, r: Ray): RGB = {

    val colorInfo = hit.color
    val baseColor: RGB = colorInfo.color
    val backFace = (hit.normal * r.direction) >  0


    //ambient
    val ambientColor = if(!backFace) baseColor * colorInfo.ambient else Renderer.backgroundColor

    //visible lights
    val visibleLights = scene.lights.filter( l => !backFace && !shadowRay(hit.position, l.position) )

    //diffuse
    val diffuseColor = visibleLights.map{ l => {
        val L = (l.position-hit.position).normalized // vector pointing towards light //TODO duplicate calculation
        baseColor * Math.max((hit.normal * L),0) * colorInfo.diffuse * l.intensity //TODO light color?
      }
    } match {
      case Seq() => RGB.BLACK
      case list => list.reduce(_ + _)
    }


    //specular
    val specColor=  visibleLights.map{ l => {
        val V = r.direction * - 1 //towards eye
        val L = (l.position - hit.position).normalized // vector pointing towards light
        val R = V - hit.normal * (V * hit.normal) * 2 //reflected ray
        l.color* Math.pow (Math.max (- (R * L), 0), colorInfo.shininess).toFloat * colorInfo.spec * l.intensity //spec does not use color of object
      }
    } match {
      case Seq() => RGB.BLACK
      case list => list.reduce(_ + _)
    }

    //reflection
    val reflectedColor : RGB = if(Math.pow(colorInfo.reflective,r.depth+1)> 0.00001 && r.depth <= 6) //TODO make configurable
      traceRay( r.reflectedAt(hit.position, hit.normal) ) * colorInfo.reflective
    else RGB.BLACK

    //refracted ray
    val refractedColor =
      if(colorInfo.refractive >0.00001 &&  r.depth <= 6) {
        //TODO configurable ray depth
        r.refractedAt(hit.position, hit.normal, colorInfo.n) match {
          case Some(refractedRay) => traceRay(refractedRay) * colorInfo.refractive
          case None               => Renderer.backgroundColor
        }

      }else {
        Renderer.backgroundColor
      }
    val combinedExposure : RGB = ambientColor+diffuseColor+specColor+refractedColor+reflectedColor
    combinedExposure.exposureCorrected.gammaCorrected
  }

  def traceRay(r: Ray): RGB =
    getFirstHit(r) match {
      case Some(hit) => shadeHit(hit,r)
      case _         => Renderer.backgroundColor
    }


  def getFirstHit(r:Ray) : Option[Hit] = scene.shapes.flatMap{ s => s intersect r } match {
    case Nil => None
    case xs => Some(xs.minBy(_.distance))
  }

  def anyHit(r:Ray, maxDist: Float ) : Boolean = scene.shapes.exists{ s => s.intersect(r,maxDist) }

  def render(config: Config) = {

    val start = System.nanoTime()
    logger.info("Starting to trace")
    val img = new Image((scene.width*scene.ppi).toInt,(scene.height*scene.ppi).toInt)
    val w: Float = scene.width
    val h: Float = scene.height
    val corner =  scene.cameraOrigin + scene.cameraPointing - scene.side*(w/2) + scene.up*(h/2)

    val X = img.width
    val Y = img.height

    val renderStart = System.nanoTime()

    val iter : Iterator[(Int,Int)] =
      for {x <- 0 until X iterator;
           y <- 0 until Y iterator}
       yield (x,y)
    val rand = new Random(0)

    logger.info(s"tracing ${config.supersampling}x${config.supersampling} per pixel")
    iter.toStream.par foreach{//TODO: try to re// nder patches => less overhead for parallelizing
      case (x:Int,y:Int) => {
        //supersampling //TODO: could be made adaptive //could use multijitter
        val S = config.supersampling*config.supersampling
        val colorSum : RGB =
          (for{i <- 0 until config.supersampling
               j <- 0 until config.supersampling
               shift = (config.supersampling-1)/(2f*config.supersampling)
              rayTarget =  (corner
                  + scene.side*((w*(x+i.toFloat/config.supersampling-shift))/X)
                  - scene.up  *(h*(y+j.toFloat/config.supersampling-shift)/Y))
              rayDir = (rayTarget - scene.cameraOrigin).normalized
            } yield traceRay(Ray(scene.cameraOrigin, rayDir))
          ).reduce(_ + _)
        img.set(x, y, (colorSum/S).awtColor() )
      }
    }
    val now = System.nanoTime()
    logger.info(s"Raytracing done in ${(now-renderStart)/(1000f*1000*1000)} seconds") //TODO nice time formatter

    val saved = img.save(config.out)
    logger.info(s"Total runtime: ${(System.nanoTime()-start)/(1000f*1000*1000)} seconds")
  }

}
