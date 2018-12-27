name := "ScaSMATA"

version := "0.2"

mainClass in (Compile,run) := Some("org.scasmata.util.Main")
mainClass in assembly := Some("org.scasmata.util.Main")


resolvers += "Artifactory-UCL" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2",
  "com.typesafe.akka" %% "akka-actor" % "2.5.11",
  "com.typesafe.akka" %% "akka-remote" % "2.5.11"
)

