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
  "com.typesafe.play" %% "play-json" % "2.3.4")

scalacOptions in Test ++= Seq("-Yrangepos")
    