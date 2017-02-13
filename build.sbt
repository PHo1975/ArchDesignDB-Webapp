enablePlugins(ScalaJSPlugin)
lazy val root:Project= (project in file(".")).settings(
  name:="ClientBase",
  version:="0.1",
  scalaVersion:="2.12.1",
  scalacOptions ++= Seq( "-deprecation"),
  scalaJSStage in Global := FastOptStage,
  artifactPath in (Compile,fastOptJS) := file("E:\\Programmierung\\Workplace\\ScalaBase\\deploy\\files\\main.js")
)
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
libraryDependencies +=  "dbdef" %%% "dbdef" % "0.9-SNAPSHOT"
libraryDependencies += "jsbase" %%% "jsbase" % "0.1-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5-SNAPSHOT"
//libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.6"
//libraryDependencies += "org.scala-js" %% "scalajs-javalib-ex" % "0.6.13"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.2"
addCommandAlias("f", "fastOptJS")


