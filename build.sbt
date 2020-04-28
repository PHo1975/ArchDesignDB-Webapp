
lazy val root:Project= (project in file(".")).settings(
  name:="ClientBase",
  version := "0.1.2",
  scalaVersion := "2.13.1",
  scalacOptions ++= Seq( "-deprecation"),
  //scalacOptions ++= Seq( "-deprecation -P:scalajs:sjsDefinedByDefault"),
  scalaJSStage in Global := FastOptStage,
  artifactPath in (Compile,fastOptJS) := file("/media/platted/Programmdaten/Scalabase/deploy/Files/main.js"),
  artifactPath in (Compile,fullOptJS) := file("/media/platted/Programmdaten/Scalabase/deploy/Files/main.js")

).enablePlugins(ScalaJSPlugin)
scalaJSLinkerConfig := {
  val fastOptJSURI = (artifactPath in (Compile, fastOptJS)).value.toURI
  scalaJSLinkerConfig.value.withRelativizeSourceMapBase(Some(fastOptJSURI))
}
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"
libraryDependencies +=  "dbdef" %%% "dbdef" % "0.9-SNAPSHOT"
libraryDependencies += "jsbase" %%% "jsbase" % "0.1-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.8.2"
libraryDependencies += "threejs-facade" %%% "threejs-facade" % "0.1-SNAPSHOT"
//libraryDependencies += "io.github.cquiroz" %%% "scala-java-locales" % "0.5.3-cldr31"
updateOptions := updateOptions.value.withLatestSnapshots(false)
addCommandAlias("f", "fastOptJS")
scalaJSUseMainModuleInitializer := true


