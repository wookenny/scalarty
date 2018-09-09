import org.specs2.{ScalaCheck, Specification}
import support.{Config, SamplingValue}

class MainSpec extends Specification with ScalaCheck {

  def is = s2"""
    Main provides a parser that
      fails if input scene missed $failingWithoutInputFile
      fails if output image name missed $failingWithoutOutputFile
      succeeds if input scene and output image name are specified $testRequireInputFile

      parse supersampling correctly ${testParsingSupersampling(Seq(("1:4",SamplingValue(1,4)),
                                                                   ("2:3",SamplingValue(2,3)),
                                                                   (":3", SamplingValue(1,3)),
                                                                   ("7",  SamplingValue(7,0)),
                                                                   ("3:0",SamplingValue(3,0)),
                                                                   ("4:4",SamplingValue(4,0)),
                                                                   ("8:5",SamplingValue(8,0)),
                                                                   ("0",  SamplingValue(1,0))
                                                              ))}
      fail parsing supersampling ${testParsingInvalidSupersampling(Seq("-1", "noValidValue", "1:4:45"))}

      parse shadowsampling correctly ${testParsingSupersampling(Seq(("1:4", SamplingValue(1, 4)),
                                                                    ("2:3", SamplingValue(2, 3)),
                                                                    (":3",  SamplingValue(1, 3)),
                                                                    ("7",   SamplingValue(7, 0)),
                                                                    ("3:0", SamplingValue(3, 0)),
                                                                    ("4:4", SamplingValue(4, 0)),
                                                                    ("8:5", SamplingValue(8, 0)),
                                                                    ("0",   SamplingValue(1, 0))
                                                               ))}
      fail parsing shadowsampling ${testParsingInvalidShadowsampling(Seq("-1", "noValidValue", "1:4:45"))}

      parse flag bvh.showleafes correctly ${testFlag("--bvh.showleaves",_.showBvHLeaves)}
      parse flag bvh.sl correctly ${testFlag("-bvh.l", _.showBvHLeaves)}
      parse flag debug correctly ${testFlag("--debug", _.debug)}
      parse flag verbose correctly ${testFlag("--verbose", _.verbose)}
      parse flag sah correctly ${testNegatedFlag("--nosah", _.sah)}


    """

  val validInput = Seq("-i", "Scene.json", "-o", "RenderedImage.png")
  val validConfig = Config().copy(in = "Scene.json", out = "RenderedImage.png")


  def failingWithoutInputFile  = parse(Seq("-o", "RenderedImage.png")) should beNone
  def failingWithoutOutputFile = parse(Seq("-i", "Scene.json")) should beNone
  def testRequireInputFile = parse(validInput) should beSome(===(validConfig))


  def testFlag(flag: String, field: Config => Boolean) = {

    (parse(validInput).map(field) should beSome(===(false))) and
      (parse(validInput ++ Seq(flag)).map(field) should beSome(===(true)))
  }

  def testNegatedFlag(flag: String, field: Config => Boolean) = {

    (parse(validInput).map(field) should beSome(===(true))) and
      (parse(validInput ++ Seq(flag)).map(field) should beSome(===(false)))
  }



  def testParsingSamplingGeneral(checkPairs : Seq[(String,SamplingValue)],flags : Seq[String], field: Config => SamplingValue) = (for {
  (input, result) <- checkPairs
    flag <-flags
  } yield parse(validInput ++ Seq(flag,input)).map(_.supersampling) should beSome(===(result)))
    .reduce(_ and _)

  def testParsingSupersampling(pairs : Seq[(String,SamplingValue)]) =
    testParsingSamplingGeneral(pairs, Seq("-s","--supersampling"),_.supersampling)

  def testParsingShadowsampling(pairs : Seq[(String,SamplingValue)]) =
    testParsingSamplingGeneral(pairs, Seq("-shs","--shadowsampling"),_.shadowsampling)



  def testParsingInvalidSamplingGeneral(values : Seq[String],flags : Seq[String], field: Config => SamplingValue) = (for{
    input <- values
    flag <- flags
  } yield parse(validInput ++ Seq(flag, input)).map(field) should beNone)
    .reduce(_ and _)

  def testParsingInvalidSupersampling(values : Seq[String]) = testParsingInvalidSamplingGeneral(values,Seq("-s","--supersampling"),_.supersampling)

  def testParsingInvalidShadowsampling(values : Seq[String]) = testParsingInvalidSamplingGeneral(values,Seq("-shs","--shadowsampling"),_.shadowsampling)


  /* TODO: Test this
  "bvh.splitlimit"
  */
  def parse(args: Seq[String]): Option[Config] = Main.parser.parse(args, Config())

}
