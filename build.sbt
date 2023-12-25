val scala3Version = "3.3.1"
val zioVersion = "2.0.20"

ThisBuild / scalaVersion := scala3Version

ThisBuild / libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-streams" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2023"
  )
  .aggregate(day_1_trebuchet)

lazy val day_1_trebuchet = project
  .in(file("day-1-trebuchet"))
  .settings(name := "day-1-trebuchet")
