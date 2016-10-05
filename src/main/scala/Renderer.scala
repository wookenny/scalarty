import java.awt.Color
import java.util.Random

import com.typesafe.scalalogging._
import geometry.Light
import geometry.{Hit, Ray, Vector3}

import scala.Seq
import scala.collection.parallel.ParSeq

object Renderer{
  val backgroundColor = Vector3.ZERO
  val gamma : Float = 2.2f
}

class Renderer(val scene: Scene) extends LazyLogging {

  private def toColor(colorVec: Vector3): Color = {
    new Color( Math.min(Math.max(0f,colorVec.x),1f),
               Math.min(Math.max(0f,colorVec.y),1f),
               Math.min(Math.max(0f,colorVec.z),1f))
  }

  def shadowRay(position: Vector3, light: Vector3): Boolean = {
      val vectorToLight = (light-position)
      anyHit( Ray(position, vectorToLight.normalized), vectorToLight.length )
  }

  def shadePixel(hit: Hit, r: Ray): Vector3 = {

    val colorInfo = hit.color
    val baseColor : Vector3 = Vector3(colorInfo.color.getRed/255f, colorInfo.color.getGreen/255f, colorInfo.color.getBlue/255f)

    //ambient
    val ambientColor : Vector3 = baseColor * colorInfo.ambient

    //visible lights
    val visibleLights = scene.lights.filter( l => !shadowRay(hit.position, l.position) )

    //diffuse
    val diffuseColor = visibleLights.map{ l => {
        val L = (l.position-hit.position).normalized // vector pointing towards light //TODO duplicate calculation
        baseColor * Math.max((hit.normal * L),0) * colorInfo.diffuse * l.intensity //TODO light color?
      }
    } match {
      case Seq() => Vector3.ZERO
      case list => list.reduce(_ + _)
    }


    //specular
    val specColor =  visibleLights.map{ l => {
        val V = r.direction * - 1 //towards eye
        val L = (l.position - hit.position).normalized // vector pointing towards light
         val R = V - hit.normal * (V * hit.normal) * 2 //reflected ray
         baseColor * Math.pow (Math.max (- (R * L), 0), colorInfo.shininess).toFloat * colorInfo.spec * l.intensity //TODO light color?}
      }
    } match {
      case Seq() => Vector3.ZERO
      case list => list.reduce(_ + _)
    }

    //

    val combinedExposure = ambientColor+diffuseColor+specColor
    val corrected = Vector3(1,1,1) - (combinedExposure* -1).expf
    corrected.pow(1/Renderer.gamma) //gamma correction
  }

  def renderPixel(x: Int, y: Int, r: Ray): Vector3 = {
    val hit = getFirstHit(r)
    if(!hit.isDefined)
        Renderer.backgroundColor
    else
        shadePixel(hit.get, r)
  }

  def getFirstHit(r:Ray) : Option[Hit] = scene.shapes.flatMap{ s => s intersect r } match {
    case Nil => None
    case xs => Some(xs.minBy(_.distance))
  }

  def anyHit(r:Ray, maxDist: Float ) : Boolean = scene.shapes.exists{ s => s.intersect(r,maxDist) }

  def render(config: Config) = {

    val start = System.nanoTime()
    logger.info("Starting to trace")
    val img = new Image(scene.width*scene.ppi,scene.height*scene.ppi)
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
        val colorSum : Vector3 =
          (for{i <- 0 until config.supersampling
               j <- 0 until config.supersampling
               shift = (config.supersampling-1)/(2f*config.supersampling)
              rayTarget =  (corner
                  + scene.side*((w*(x+i.toFloat/config.supersampling-shift))/X)
                  - scene.up  *(h*(y+j.toFloat/config.supersampling-shift)/Y))
              rayDir = (rayTarget - scene.cameraOrigin).normalized
            } yield renderPixel(x,y, Ray(scene.cameraOrigin, rayDir))
          ).reduce(_ + _)
        img.set(x, y, toColor(colorSum/S))
      }
    }
    val now = System.nanoTime()
    logger.info(s"Raytracing done in ${(now-renderStart)/(1000f*1000*1000)} seconds") //TODO nice time formatter

    img.save(config.out)
    logger.info(s"Total runtime: ${(System.nanoTime()-start)/(1000f*1000*1000)} seconds")
  }

}
