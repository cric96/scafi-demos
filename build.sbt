name := "scafi-experiment"

version := "0.1"

scalaVersion := "2.12.4"
// build.sbt
val scafi_version = "v0.3.3"


val scafi_core  =  "it.unibo.apice.scafiteam" %% "scafi-core" % scafi_version
val scafi_simulator  =  "it.unibo.apice.scafiteam" %% "scafi-simulator" % scafi_version
val scafi_simulator_gui =  "it.unibo.apice.scafiteam" %% "scafi-simulator-gui-new" % scafi_version intransitive()
val scalafx = "org.scalafx" %% "scalafx" % "10.0.2-R15"

libraryDependencies ++= Seq(scafi_simulator_gui, scafi_core, scafi_simulator, scalafx)

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
lazy val javaFXModules = Seq("base", "controls", "graphics", "media", "swing", "web")
lazy val javaVersion = System.getProperty("java.version").stripPrefix("openjdk")
lazy val jdkVersion = javaVersion.split('.').headOption.getOrElse(if(javaVersion.isEmpty) "11" else javaVersion)

lazy val javaFX = if(scala.util.Try(jdkVersion.toInt).getOrElse(0) >= 11) {
  javaFXModules.map(m => "org.openjfx" % s"javafx-$m" % (jdkVersion+".0.1") classifier osName)
} else {
  Seq()
}

Runtime / unmanagedClasspath += baseDirectory.value / "src" / "main" / "resources" //to solve bundle problems

libraryDependencies ++= javaFX