name := "hyper"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.11.0",
  "org.spire-math" %% "spire-laws" % "0.11.0" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0-M7" % "test")
