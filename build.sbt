enablePlugins(ScalaJSPlugin)
lazy val root:Project= (project in file(".")).settings(
  name:="ClientBase",
  version := "0.1.1",
  scalaVersion := "2.12.4",
  scalacOptions ++= Seq( "-deprecation"),
  scalaJSStage in Global := FastOptStage,
  artifactPath in (Compile,fastOptJS) := file("/home/peter/kathi/Programmdaten/Scalabase/deploy/Files/main.js")

)
scalaJSLinkerConfig := {
  val fastOptJSURI = (artifactPath in (Compile, fastOptJS)).value.toURI
  scalaJSLinkerConfig.value.withRelativizeSourceMapBase(Some(fastOptJSURI))
}
resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2"
libraryDependencies +=  "dbdef" %%% "dbdef" % "0.9-SNAPSHOT"
libraryDependencies += "jsbase" %%% "jsbase" % "0.1-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.0"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"
libraryDependencies += "org.denigma" %%% "threejs-facade" % "0.0.77-0.1.8"
addCommandAlias("f", "fastOptJS")
scalaJSUseMainModuleInitializer := true


