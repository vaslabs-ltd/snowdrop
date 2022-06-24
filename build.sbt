import Dependencies.Modules

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "snowdrop"
  ).aggregate(`snowdrop-circe`)

lazy val `snowdrop-circe` = (project in file("snowdrop-circe"))
  .settings(
    name := "snowdrop-circe"
  ).settings(
    libraryDependencies ++= Modules.`snowdrop-circe`
  )