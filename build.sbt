name := "scafi-experiment"

version := "0.1"


ThisBuild / scalaVersion := "2.13.4"
// build.sbt
val scafi_version = "1.1.6"

val scafi_simulator_gui = "it.unibo.scafi" %% "simulator-gui-new" % scafi_version

libraryDependencies ++= Seq(scafi_simulator_gui)
