ThisBuild / version := "0.1.0-SNAPSHOT"

// Use the LTS version
// https://www.scala-lang.org/download/all.html
ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "Advent2023"
  )

// https://github.com/typelevel/cats-effect/releases
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"
// https://github.com/typelevel/cats-parse/releases
libraryDependencies += "org.typelevel" %% "cats-parse" % "1.0.0"
// https://github.com/scalameta/munit/releases
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
