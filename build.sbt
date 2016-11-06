name := "Scalarty"

version := "1.0"
scalaVersion := "2.11.8"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.7" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.7" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalariform" %% "scalariform" % "0.1.8",
  "com.typesafe.play" %% "play-json" % "2.5.9")

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq("-unchecked",
                      "-deprecation",
                      "-feature",
                      "-language:postfixOps")
scalacOptions in (Compile, doc) ++= Seq("-unchecked",
                                        "-deprecation",
                                        "-diagrams",
                                        "-implicits",
                                        "-skip-packages",
                                        "samples")

coverageMinimum := 70

coverageFailOnMinimum := false

coverageHighlighting := {
  if (scalaBinaryVersion.value == "2.11")
    true
  else
    false
}

wartremoverErrors ++= Warts.allBut(Wart.Overloading,
                                   Wart.DefaultArguments,
                                   Wart.Nothing,
                                   Wart.Product,
                                   Wart.Serializable,
                                   Wart.Option2Iterable)
wartremoverWarnings ++= Warts.allBut(Wart.Overloading, Wart.DefaultArguments)
wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "AABBSpec.scala"
wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "MaterialSpec.scala"
wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "PointSpec.scala"
wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "RaySpec.scala"
wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "TriangleSpec.scala"
wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "SphereSpec.scala"

scalafmtConfig in ThisBuild := Some(file(".scalafmt.conf"))
