package support

import com.typesafe.scalalogging.Logger
import scala.concurrent.duration._


object Util{

  def time[A](msg: String)(block: => A)(implicit timelogger: Logger): A = {
    val start = System.nanoTime
    val res = block
    val elapsed = Duration.fromNanos(System.nanoTime - start)
    timelogger.info(s"$msg ${elapsed.toMillis} milliseconds")
    res
  }
}
