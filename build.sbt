import sbt.Keys.libraryDependencies

val commonSettings = Seq(
  scalaVersion := "2.13.6",
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"
)



lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
