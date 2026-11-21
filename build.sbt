scalaVersion := "3.8.4"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
Compile / classDirectory := baseDirectory.value / "target" / "classes"

Compile / compile := {
  val analysis = (Compile / compile).value
  val outDir = (Compile / classDirectory).value
  if (!(outDir / "cats" / "Monad.class").exists()) {
    (Compile / dependencyClasspath).value
      .map(_.data)
      .filter(_.getName.contains("cats"))
      .foreach(IO.unzip(_, outDir))
  }
  analysis
}