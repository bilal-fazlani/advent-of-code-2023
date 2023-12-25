val scala3Version = "3.3.1"
val zioVersion = "2.0.20"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2023",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
