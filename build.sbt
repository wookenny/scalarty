name := "Scalarty"

version := "1.0"
scalaVersion := "2.12.3"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.9.5" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.9.5" % "test",
  "org.specs2" %% "specs2-mock" % "3.9.5" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "com.typesafe.play" %% "play-json" % "2.6.5",
  "org.mockito" % "mockito-core" % "2.10.0",
  "com.google.inject" % "guice" % "4.1.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.typelevel" %% "cats-effect" % "0.4"
)

parallelExecution in Test := false

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:postfixOps")
scalacOptions in (Compile, doc) ++= Seq("-unchecked",
                                        "-deprecation",
                                        "-diagrams",
                                        "-implicits",
                                        "-skip-packages",
                                        "samples")
scalacOptions ++= scalafixScalacOptions.value

coverageMinimum := 70
coverageEnabled := true
coverageFailOnMinimum := false
//wartremoverErrors in (Compile, compile) ++= Warts.unsafe.filterNot(_==Wart.DefaultArguments)
