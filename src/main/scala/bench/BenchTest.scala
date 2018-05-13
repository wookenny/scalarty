package bench

import bounding.BVH
import io.circe.parser.decode
import io.circe.generic.auto._
import math.{AABB, Ray, Triangle, Vector3}
import support.{Config, Image}
import support.Implicits._

import scala.io.Source.fromFile
import scene.{ObjObject, Scene, SceneDTO}

object BenchTest {
  def main(args: Array[String]): Unit = {

    implicit val config = Config()

    val obj = ObjObject("scenes/obj/dragon.obj.gz", Vector3.ZERO, 2d, 0)
    val triangleSeq = obj.getTriangles.toVector

    val steps = 25
    val box = AABB.wrapping(triangleSeq).getOrElse(AABB.empty)

    val rays: Seq[Ray] = for {
      x <- 0 to steps
      y <- 0 to steps
      stepsizeX = (box.maxX - box.minX) / steps
      stepsizeY = (box.maxY - box.minY) / steps
    } yield
      Ray(Vector3(box.minX + x * stepsizeX, box.minY + y * stepsizeY, box.minZ - 1), Vector3.Z)

    val th = ichi.bench.Thyme.warmed(verbose = print)

    val benchmarkInstances = for {
      sah <- Seq(true, false)
      leafLimit <- 3 to 10
      bvh = BVH(triangleSeq, leafLimit, sah)
      w = th.Warm(rays.map(bvh.intersect).count(_.isDefined))
    } yield th.pbenchWarm(w, title = s"BVH - sah:$sah, limit:$leafLimit")
    //val result = th.pbenchOffWarm()(w1)(w2)
    //println(s"Benchmark result:\n$result")

    //benchmarkInstances.map(x => th.pbenchWarm(x))

  }

}
