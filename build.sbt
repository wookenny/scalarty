name := "Scalarty"

version := "1.0"
scalaVersion := "2.11.8"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.7" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.7" % "test",
  "org.specs2" %% "specs2-mock" % "3.7" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalariform" %% "scalariform" % "0.1.8",
  "com.typesafe.play" %% "play-json" % "2.5.9",
  "org.typelevel" %% "cats" % "0.7.2",
  "org.mockito" % "mockito-core" % "2.3.7",
  "com.google.inject" % "guice" % "3.0")

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
coverageEnabled := true
coverageFailOnMinimum := false

coverageHighlighting := {
  if (scalaBinaryVersion.value == "2.11")
    true
  else
    false
}

//wartremoverErrors in (Compile, compile) ++= Warts.unsafe.filterNot(_==Wart.DefaultArguments)
scalafmtConfig in ThisBuild := Some(file(".scalafmt.conf"))
