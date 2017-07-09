import org.specs2.Specification
import support.Util
import org.specs2.mock._

trait TimeProvider {
  def time(): Long
}

class UtilSpec extends Specification with Mockito {

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

  val executionTimeLoggedForZeroNanoseconds =
    executionTimeLogged(0, "0 nanoseconds")
  val executionTimeLoggedFor16Nanoseconds =
    executionTimeLogged(16, "16 nanoseconds")
  val executionTimeLoggedFor28microseconds =
    executionTimeLogged(28L * 1000, "28 microseconds")
  val executionTimeLoggedFor35Milliseconds =
    executionTimeLogged(35L * 1000 * 1000, "35 milliseconds")
  val executionTimeLoggedFor39Seconds =
    executionTimeLogged(39L * 1000 * 1000 * 1000, "39 seconds")
  val executionTimeLoggedFor12Minutes =
    executionTimeLogged(12L * 1000 * 1000 * 1000 * 60, "12 minutes")
  val executionTimeLoggedFor3Hours =
    executionTimeLogged(3L * 1000 * 1000 * 1000 * 60 * 60, "3 hours")

  def executionTimeLogged(elapsedTime: Long, expectedMessage: String) = {
    val timeProvider = mock[TimeProvider]
    timeProvider.time() returns 0 thenReturn elapsedTime

    val msg = "Logging msg"
    val block = { val x = 12; x }
    var logMessages: Seq[String] = Seq.empty
    val result = Util.time(msg) { block }((s: String) => logMessages = s +: logMessages,
      () => timeProvider.time())

    val messageLogged = logMessages.headOption.getOrElse("")

    (result should be equalTo block) and
      (messageLogged should contain(expectedMessage)) and (messageLogged should contain(msg))
  }
}
