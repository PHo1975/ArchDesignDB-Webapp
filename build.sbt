enablePlugins(ScalaJSPlugin)
lazy val root:Project= (project in file(".")).settings(
  name:="ClientBase",
  version := "0.1.1",
  scalaVersion := "2.12.2",
  scalacOptions ++= Seq( "-deprecation"),
  scalaJSStage in Global := FastOptStage,
  artifactPath in (Compile,fastOptJS) := file("E:\\Programmierung\\Workplace\\ScalaBase\\deploy\\files\\main.js")

)
resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2"
libraryDependencies +=  "dbdef" %%% "dbdef" % "0.9-SNAPSHOT"
libraryDependencies += "jsbase" %%% "jsbase" % "0.1-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5-SNAPSHOT"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5"
libraryDependencies += "org.denigma" %%% "threejs-facade" % "0.0.77-0.1.8"
addCommandAlias("f", "fastOptJS")
scalaJSUseMainModuleInitializer := true


