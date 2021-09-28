ThisBuild / version      := "0.0.1"
ThisBuild / organization := "bnord"
ThisBuild / scalaVersion := "3.0.2"

val scalatest = "org.scalatest" %% "scalatest" % "3.2.9"
val scalatestpluscheck = "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0"


lazy val root = (project in file("."))
  .settings(
    Compile / scalacOptions := Seq("-unchecked", "-deprecation"),
    name := "ltl2aut",
    libraryDependencies += scalatest % Test,
    libraryDependencies += scalatestpluscheck % Test
  )
