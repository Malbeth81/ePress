name := "Inpress"
organization := "com.prosapient"
version := "1.0.0"
scalaVersion := "2.11.8"

//javacOptions ++= Seq("-g:none")
//scalacOptions ++= Seq("-feature", "-g:none")
parallelExecution in ThisBuild := false
publishArtifact in packageDoc := false
publishArtifact in packageSrc := false
publishArtifact in GlobalScope in Test := true
sources in doc := Seq.empty
sourcesInBase := false

lazy val inpress = project.in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      // Test dependencies
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    )
  )