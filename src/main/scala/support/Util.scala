package support

import scala.concurrent.duration._

object Util {

  // default implicit time measurement
  implicit def currentTimeInNanos: () => Long = () => System.nanoTime()

  def time[A](msg: String)(block: => A)(implicit log: String => Unit,
                                        currentTimeInNanos: () => Long): A = {
    val start = currentTimeInNanos()
    val res = block
    log(s"$msg ${elapsedTime(start, currentTimeInNanos())}")
    res
  }

  def elapsedTime(start: Long, stop: Long) = (stop - start) nanos match {
    case e if e < (1000 nanos) => e
    case e if e < (1000 micros) => Duration(e.toMicros, MICROSECONDS)
    case e if e < (1000 millis) => Duration(e.toMillis, MILLISECONDS)
    case e if e < (180 seconds) => Duration(e.toSeconds, SECONDS)
    case e if e < (60 minutes) => Duration(e.toMinutes, MINUTES)
    case e => Duration(e.toHours, HOURS)
  }
}
