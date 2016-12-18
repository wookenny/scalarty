import org.specs2.Specification
import support.Util




class UtilSpec extends Specification {

  override def is = s2"""
      Util should track execution time correctly $executionTimeLogged

      """

  val executionTimeLogged = {

    val msg = "Logging msg"

    val block = {val x = 12; x}
    var logMessages : Seq[String] = Seq.empty
    val result = Util.time(msg){block}((s:String) => logMessages = s+:logMessages)

    result should be equalTo block
    (logMessages.headOption match {
      case Some(s) if s contains msg => true
      case _ => false
    }) should be equalTo true

  }

}
