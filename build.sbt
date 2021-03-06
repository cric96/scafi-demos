name := "scafi-experiment"

version := "0.1"

scalaVersion := "2.12.4"

// build.sbt
val scafi_version = "v0.3.3"

val scafi_core  =  "it.unibo.apice.scafiteam" %% "scafi-core" % scafi_version
val scafi_simulator  =  "it.unibo.apice.scafiteam" %% "scafi-simulator" % scafi_version
libraryDependencies ++= Seq(scafi_core, scafi_simulator)