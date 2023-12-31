ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
    .settings(
      name             := "Advent of Code 2023",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "fastparse" % "3.0.2"
      ),
    )
