import org.specs2.Specification
import support.Util

case class timeProvider(first: Long, successive: Long) {
  var called = false
  def time = if (!called) { called = true; first } else successive
}

class UtilSpec extends Specification {

  override def is =
    s2"""
      Util should execute block and track execution time correctly for
        0 nanoseconds $executionTimeLoggedForZeroNanoseconds
        16 nanoseconds $executionTimeLoggedFor16Nanoseconds
        28 microseconds $executionTimeLoggedFor28microseconds
        35 milliseconds $executionTimeLoggedFor35Milliseconds
        39 seconds $executionTimeLoggedFor39Seconds
        12 minutes $executionTimeLoggedFor12Minutes
        3 hours $executionTimeLoggedFor3Hours
      """

  private def ZeroNanoseconds         = timeProvider(0, 0).time _
  private def SixteenNanoseconds      = timeProvider(0, 16).time _
  private def TwentyEightMicroseconds = timeProvider(0, 28 * 1000).time _
  private def ThirtyFiveMilliseconds  = timeProvider(0, 35 * 1000 * 1000).time _
  private def ThirtyNineSeconds       = timeProvider(0, 39 * 1000 * 1000 * 1000).time _
  private def TwelveMinutes           = timeProvider(0, 12 * 1000 * 1000 * 1000 * 60).time _
  private def ThreeHours              = timeProvider(0, 39 * 1000 * 1000 * 1000 * 60 * 60).time _

  val executionTimeLoggedForZeroNanoseconds = executionTimeLogged(ZeroNanoseconds, "0 nanoseconds")
  val executionTimeLoggedFor16Nanoseconds   =  executionTimeLogged(ZeroNanoseconds, "16 nanoseconds")
  val executionTimeLoggedFor28microseconds  = executionTimeLogged(TwentyEightMicroseconds, "28 microseconds")
  val executionTimeLoggedFor35Milliseconds  = executionTimeLogged(ThirtyFiveMilliseconds, "35 milliseconds")
  val executionTimeLoggedFor39Seconds       = executionTimeLogged(ThirtyNineSeconds, "39 seconds")
  val executionTimeLoggedFor12Minutes       = executionTimeLogged(TwelveMinutes, "12 minutes")
  val executionTimeLoggedFor3Hours          = executionTimeLogged(ThreeHours, "3 hours")

  def executionTimeLogged(time: () => Long,  expectedMessage: String) = {

    val msg = "Logging msg"
    val block = { val x = 12; x}
    var logMessages: Seq[String] = Seq.empty
    val result = Util.time(msg) {block}((s: String) => logMessages = s +: logMessages,  time)

    val messageLogged = logMessages.headOption.getOrElse("")

    result should be equalTo block
    messageLogged must contain(expectedMessage)
    messageLogged must contain(msg)
  }
}
