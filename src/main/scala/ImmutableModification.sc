import monocle.PLens

case class ConfigParam(a:Int,b:String)
case class AdditionalConfigParam(c:Double,d:Seq[Float])

case class Settings(configParam:ConfigParam, additionalConfigParam: AdditionalConfigParam)

val s = Settings(ConfigParam(3,"rt"), AdditionalConfigParam(2.87, Seq(0.3f,-2.1f)))


//copy
def setD(s: Settings, new_d: Seq[Float]) = s.copy(additionalConfigParam = s.additionalConfigParam.copy(d = new_d))
val s1 = setD(s, Seq(1.11f, 2.22f))


//monocle
import monocle.Lens
import monocle.macros.GenLens
val additionalConfigParam: Lens[Settings, AdditionalConfigParam]  = GenLens[Settings](_.additionalConfigParam)
val d: Lens[AdditionalConfigParam, Seq[Float]]  = GenLens[AdditionalConfigParam](_.d)


def setDWithLens(s: Settings, new_d: Seq[Float]) = (additionalConfigParam composeLens d).set(new_d)(s)

val dOfSettings = additionalConfigParam ^|alastcle-> d
dOfSettings.modify(_.reverse)(s)
dOfSettings.set(Seq(1.11f, 2.22f))(s)
//dOfSettings.