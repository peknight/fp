name := "pek-fp"

organization := "com.peknight"

version := "0.1"

scalaVersion := "2.13.0"

lazy val fp = (project in file("."))
    .enablePlugins(JavaAppPackaging)
    .settings(
      name := "pek-fp",
      libraryDependencies +=
        "org.typelevel" %% "cats-core" % "2.0.0",
      scalacOptions ++= Seq(
        "-Xfatal-warnings",
        "-language:higherKinds",
        "-deprecation"
      )
    )

