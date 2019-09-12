
lazy val root:Project= (project in file(".")).settings(
  name:="ClientBase",
  version := "0.1.1",
  scalaVersion := "2.12.9",
  //scalacOptions ++= Seq( "-deprecation -P:scalajs:sjsDefinedByDefault"),
  scalaJSStage in Global := FastOptStage,
  artifactPath in (Compile,fastOptJS) := file("/media/platted/Programmdaten/Scalabase/deploy/Files/main.js")
).enablePlugins(ScalaJSPlugin)
scalaJSLinkerConfig := {
  val fastOptJSURI = (artifactPath in (Compile, fastOptJS)).value.toURI
  scalaJSLinkerConfig.value.withRelativizeSourceMapBase(Some(fastOptJSURI))
}
resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2"
libraryDependencies +=  "dbdef" %%% "dbdef" % "0.9-SNAPSHOT"
libraryDependencies += "jsbase" %%% "jsbase" % "0.1-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.8"
libraryDependencies += "org.denigma" %%% "threejs-facade" % "0.0.77-0.1.8"
libraryDependencies += "io.github.cquiroz" %%% "scala-java-locales" % "0.5.3-cldr31"
updateOptions := updateOptions.value.withLatestSnapshots(false)
addCommandAlias("f", "fastOptJS")
scalaJSUseMainModuleInitializer := true


