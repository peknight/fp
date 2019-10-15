name := "pek-fp"

organization := "com.peknight"

version := "0.1"

scalaVersion := "2.13.1"

lazy val fp = (project in file("."))
    .enablePlugins(JavaAppPackaging)
    .settings(
      name := "pek-fp",
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.0.0",
        "org.scalacheck" %% "scalacheck" % "1.14.1"
      ),
      scalacOptions ++= Seq(
        "-Xfatal-warnings",
        "-language:higherKinds",
        "-deprecation",
        "-feature"
      )
    )

