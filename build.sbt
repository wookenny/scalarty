name := "Scalarty"

version := "1.0"
scalaVersion := "2.12.5"


resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val specs2Version = "4.0.3"
val circeVersion = "0.9.3"



libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % specs2Version % "test",
  "org.specs2" %% "specs2-scalacheck" % specs2Version % "test",
  "org.specs2" %% "specs2-mock" % specs2Version % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic"  % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion,
  "org.mockito" % "mockito-core" % "2.18.0",
  "com.google.inject" % "guice" % "4.2.0",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.typelevel" %% "cats-effect" % "0.10",
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-natives" % "0.13.2"
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

coverageMinimum := 70
coverageEnabled := true
coverageFailOnMinimum := false
//wartremoverErrors in (Compile, compile) ++= Warts.unsafe.filterNot(_==Wart.DefaultArguments)
